#' Scrape Radar Jawa Pos Data
#'
#' This function scrapes economic news data from the Radar Kediri Jawa Pos website for a specific region and a specified number of pages.
#'
#' @name scraperadar
#' @param wilayahradar A character string indicating the specific Radar Jawa Pos region (e.g., "kediri").
#' @param x An integer specifying the number of pages to scrape.
#' @return A data frame containing the scraped data, including titles, dates, and links for economic news.
#'
#' @import readxl
#' @import dplyr
#' @import stringr
#' @import openxlsx
#' @import tidyverse
#' @import rvest
#' @import lubridate
#' @import tidyr
#' @import tibble
#' @import utils
#' @importFrom dplyr filter group_by left_join mutate select summarise
#' @importFrom stringr str_squish str_to_title
#' @importFrom rvest read_html html_nodes html_text html_attr
#' @importFrom lubridate dmy
#' @importFrom openxlsx write.xlsx
#' @export
#'
#' @examples
#' # Scrape 2 pages of economic news from Radar Kediri
#' scraperadar("kediri", 2)
utils::globalVariables(c("katakunci", "kategori", "kategori2",
                         "tanggal", "lokasi", "dptisi", "read_html",
                         "html_nodes", "%>%", "html_attr",
                         "mutate", "dmy", "separate_rows",
                         "left_join", "filter", "group_by",
                         "summarise", "write.xlsx", "str_squish",
                         "str_to_title", "html_text", "select"))
tools::showNonASCIIfile("R/scraperadar.R")

scraperadar <- function(wilayahradar, x) {
  dptisi <- function(url) {
    x <- read_html(url) %>% html_nodes('p') %>% html_text() %>% paste(collapse = "")
    return(x)
  }

  if (wilayahradar == "kediri") {
    # Function to extract text from each page

    # Initialize an empty data frame
    radartuban <- data.frame()

    # Loop through pages
    for(i in seq(1, x, 1)){
      url <- paste0("https://radarkediri.jawapos.com/ekonomi?page=", i)
      laman <- read_html(url)

      judul <- laman %>% html_nodes('.latest__link') %>% html_text()
      tglberita <- laman %>% html_nodes('.latest__date') %>% html_text()
      link_judul <- laman %>% html_nodes('.latest__link') %>% html_attr("href")

      isiberita <- sapply(link_judul, FUN = dptisi, USE.NAMES = F)

      radartuban <- rbind(radartuban, data.frame(judul, tglberita, isiberita, link_judul))
      print(paste("Page ke-", i))
    }

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("Januari", "January", tglberita)) %>%
      mutate(tanggal = gsub("Februari", "February", tanggal)) %>%
      mutate(tanggal = gsub("Maret", "March", tanggal)) %>%
      mutate(tanggal = gsub("Mei", "May", tanggal)) %>%
      mutate(tanggal = gsub("Juni", "June", tanggal)) %>%
      mutate(tanggal = gsub("Juli", "July", tanggal)) %>%
      mutate(tanggal = gsub("Agustus", "August", tanggal)) %>%
      mutate(tanggal = gsub("Oktober", "October", tanggal)) %>%
      mutate(tanggal = gsub("Desember", "December", tanggal))

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("^\\w+, ", "", tanggal),  # Menghapus nama hari
             tanggal = gsub("[|].*", "", tanggal),  # Menghapus bagian setelah tanggal
             tanggal = format(dmy(tanggal), "%Y-%m-%d"))

    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun|Meningkat|Digelontor|Untung|Dorong|Mendorong|Cepat|Pesat|Laris|Optimis|Murah|Mudah|Permudah", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun|Berat|Melorot|Lesuh|Rugi|Buntung|Merugi|Lambat|Lamban|Sulit|Susah|Meleset", judul), -1, 0))
      )

    radartuban$lokasi <- ifelse(grepl("Kota", radartuban$isiberita, ignore.case = TRUE), "Kota Kediri", "Kediri")

    #hapus duplikat
    judulku <- radartuban$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas"
    )


    radartuban %>%
      mutate(katakunci = isiberita) %>%
      separate_rows(katakunci, sep = ' ') %>%
      mutate(katakunci = tolower(katakunci)) %>%
      left_join(katelapus) %>%
      filter(!is.na(kategori)) %>%
      select(-katakunci) -> judulku2

    judulku2 %>%
      group_by(judul) %>%
      summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                kategori = toString(kategori),
                .groups = "drop") %>%
      select(judul, kategori, kategori2) -> oke

    #hapus duplikat
    judulku2 <- judulku2[!duplicated(judulku2$judul),]

    # berita fixed
    radartuban <- judulku2[,-2]
    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun", judul), -1, 0))
      )
    names(radartuban) <- c("judul", "isiberita", "link", "tanggal", "sentimen", "lokasi", "estimasi lapus")
    radarkediri <- radartuban
    print(radarkediri)
    write.xlsx(radarkediri, paste0("radarkediri_", Sys.Date(),".xlsx"))
    return(radartuban)

  } else if (wilayahradar == "surabaya") {

    radartuban <- data.frame()
    for(i in seq(1, x, 1)){
      url <- paste0("https://radarsurabaya.jawapos.com/search?q=ekonomi&sort=latest&page=",i)
      laman <- read_html(url)
      judul <- laman %>% html_nodes('.latest__link') %>% html_text()
      tglberita <- laman %>% html_nodes('.latest__date') %>% html_text()
      link_judul <- laman %>% html_nodes('.latest__link') %>% html_attr("href")

      isiberita <- sapply(link_judul, FUN = dptisi, USE.NAMES = F)
      radartuban <- rbind(radartuban, data.frame(judul, tglberita, isiberita, link_judul))
      print(paste("Page ke-", i))
    }

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("Januari", "January", tglberita)) %>%
      mutate(tanggal = gsub("Februari", "February", tanggal)) %>%
      mutate(tanggal = gsub("Maret", "March", tanggal)) %>%
      mutate(tanggal = gsub("Mei", "May", tanggal)) %>%
      mutate(tanggal = gsub("Juni", "June", tanggal)) %>%
      mutate(tanggal = gsub("Juli", "July", tanggal)) %>%
      mutate(tanggal = gsub("Agustus", "August", tanggal)) %>%
      mutate(tanggal = gsub("Oktober", "October", tanggal)) %>%
      mutate(tanggal = gsub("Desember", "December", tanggal))

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("^\\w+, ", "", tanggal),  # Menghapus nama hari
             tanggal = gsub("[|].*", "", tanggal),  # Menghapus bagian setelah tanggal
             tanggal = format(dmy(tanggal), "%Y-%m-%d"))

    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun|Meningkat|Digelontor|Untung|Dorong|Mendorong|Cepat|Pesat|Laris|Optimis|Murah|Mudah|Permudah", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun|Berat|Melorot|Lesuh|Rugi|Buntung|Merugi|Lambat|Lamban|Sulit|Susah|Meleset", judul), -1, 0))
      )

    radartuban$lokasi <- ifelse(grepl("Surabaya", radartuban$isiberita, ignore.case = TRUE), "Kota Surabaya", NA)

    #hapus duplikat
    judulku <- radartuban$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas"
    )

    radartuban %>%
      mutate(katakunci = isiberita) %>%
      separate_rows(katakunci, sep = ' ') %>%
      mutate(katakunci = tolower(katakunci)) %>%
      left_join(katelapus) %>%
      filter(!is.na(kategori)) %>%
      select(-katakunci) -> judulku2

    judulku2 %>%
      group_by(judul) %>%
      summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                kategori = toString(kategori),
                .groups = "drop") %>%
      select(judul, kategori, kategori2) -> oke

    #hapus duplikat
    judulku2 <- judulku2[!duplicated(judulku2$judul),]

    # berita fixed
    radartuban <- judulku2[,-2]
    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun", judul), -1, 0))
      )
    names(radartuban) <- c("judul", "isiberita", "link", "tanggal", "sentimen", "lokasi", "estimasi lapus")
    radarsurabaya <- radartuban
    print(radarsurabaya)
    write.xlsx(radarsurabaya, paste0("radarsurabaya_",Sys.Date(),".xlsx" ))
    return(radartuban)
  } else if (wilayahradar == "madura") {

    radartuban <- data.frame()
    for(i in seq(1, x, 1)){
      url <- paste0("https://radarmadura.jawapos.com/search?q=ekonomi&sort=latest&page=",i)
      laman <- read_html(url)
      judul <- laman %>% html_nodes('.latest__link') %>% html_text()
      tglberita <- laman %>% html_nodes('.latest__date') %>% html_text()
      link_judul <- laman %>% html_nodes('.latest__link') %>% html_attr("href")

      isiberita <- sapply(link_judul, FUN = dptisi, USE.NAMES = F)
      radartuban <- rbind(radartuban, data.frame(judul, tglberita, isiberita, link_judul))
      print(paste("Page ke-", i))
    }


    radartuban <- radartuban %>%
      mutate(tanggal = gsub("Januari", "January", tglberita)) %>%
      mutate(tanggal = gsub("Februari", "February", tanggal)) %>%
      mutate(tanggal = gsub("Maret", "March", tanggal)) %>%
      mutate(tanggal = gsub("Mei", "May", tanggal)) %>%
      mutate(tanggal = gsub("Juni", "June", tanggal)) %>%
      mutate(tanggal = gsub("Juli", "July", tanggal)) %>%
      mutate(tanggal = gsub("Agustus", "August", tanggal)) %>%
      mutate(tanggal = gsub("Oktober", "October", tanggal)) %>%
      mutate(tanggal = gsub("Desember", "December", tanggal))

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("^\\w+, ", "", tanggal),  # Menghapus nama hari
             tanggal = gsub("[|].*", "", tanggal),  # Menghapus bagian setelah tanggal
             tanggal = format(dmy(tanggal), "%Y-%m-%d"))

    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun|Meningkat|Digelontor|Untung|Dorong|Mendorong|Cepat|Pesat|Laris|Optimis|Murah|Mudah|Permudah", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun|Berat|Melorot|Lesuh|Rugi|Buntung|Merugi|Lambat|Lamban|Sulit|Susah|Meleset", judul), -1, 0))
      )

    radartuban$lokasi <- ifelse(grepl("Pamekasan", radartuban$isiberita, ignore.case = TRUE), "Pamekasan",
                                ifelse(grepl("Sampang", radartuban$isiberita, ignore.case = TRUE), "Sampang",
                                       ifelse(grepl("Bangkalan", radartuban$isiberita, ignore.case = TRUE), "Bangkalan",
                                              ifelse(grepl("Sumenep", radartuban$isiberita, ignore.case = TRUE), "Sumenep", NA))))

    #hapus duplikat
    judulku <- radartuban$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas"
    )

    radartuban %>%
      mutate(katakunci = isiberita) %>%
      separate_rows(katakunci, sep = ' ') %>%
      mutate(katakunci = tolower(katakunci)) %>%
      left_join(katelapus) %>%
      filter(!is.na(kategori)) %>%
      select(-katakunci) -> judulku2

    #hapus duplikat
    judulku2 <- judulku2[!duplicated(judulku2$judul),]

    # berita fixed
    radartuban <- judulku2[,-2]
    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun", judul), -1, 0))
      )
    names(radartuban) <- c("judul", "isiberita", "link", "tanggal", "sentimen", "lokasi", "estimasi lapus")

    radarmadura <- radartuban
    print(radarmadura)
    write.xlsx(radarmadura, paste0("radarmadura_",Sys.Date(),".xlsx" ))
  } else if(wilayahradar == "semarang"){

    radartuban <- data.frame()
    for(i in seq(1, x, 1)){
      url <- paste0("https://radarsemarang.jawapos.com/search?q=ekonomi&sort=latest&page=",i)
      laman <- read_html(url)
      judul <- laman %>% html_nodes('.latest__link') %>% html_text()
      tglberita <- laman %>% html_nodes('.latest__date') %>% html_text()
      link_judul <- laman %>% html_nodes('.latest__link') %>% html_attr("href")

      isiberita <- sapply(link_judul, FUN = dptisi, USE.NAMES = F)
      radartuban <- rbind(radartuban, data.frame(judul, tglberita, isiberita, link_judul))
      print(paste("Page ke-", i))
    }

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("Januari", "January", tglberita)) %>%
      mutate(tanggal = gsub("Februari", "February", tanggal)) %>%
      mutate(tanggal = gsub("Maret", "March", tanggal)) %>%
      mutate(tanggal = gsub("Mei", "May", tanggal)) %>%
      mutate(tanggal = gsub("Juni", "June", tanggal)) %>%
      mutate(tanggal = gsub("Juli", "July", tanggal)) %>%
      mutate(tanggal = gsub("Agustus", "August", tanggal)) %>%
      mutate(tanggal = gsub("Oktober", "October", tanggal)) %>%
      mutate(tanggal = gsub("Desember", "December", tanggal))

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("^\\w+, ", "", tanggal),  # Menghapus nama hari
             tanggal = gsub("[|].*", "", tanggal),  # Menghapus bagian setelah tanggal
             tanggal = format(dmy(tanggal), "%Y-%m-%d"))

    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun|Meningkat|Digelontor|Untung|Dorong|Mendorong|Cepat|Pesat|Laris|Optimis|Murah|Mudah|Permudah", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun|Berat|Melorot|Lesuh|Rugi|Buntung|Merugi|Lambat|Lamban|Sulit|Susah|Meleset", judul), -1, 0))
      )

    radartuban <- radartuban %>%
      mutate(lokasi = gsub("\u2013.*|\u2014.*|-.*|, \\d{1,2} \\w+ \\d{4}", "", isiberita)) %>%
      mutate(lokasi = gsub("RADARSEMARANG.ID,\\s*|[,]|\\s*RADARSEMARANG.ID,", "", lokasi)) %>%
      mutate(lokasi = str_squish(lokasi)) %>%
      mutate(lokasi = str_to_title(lokasi)) %>%
      mutate(lokasi = gsub(" \\u2013 .*", "", lokasi))

    #hapus duplikat
    judulku <- radartuban$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas"
    )
    radartuban %>%
      mutate(katakunci = isiberita) %>%
      separate_rows(katakunci, sep = ' ') %>%
      mutate(katakunci = tolower(katakunci)) %>%
      left_join(katelapus) %>%
      filter(!is.na(kategori)) %>%
      select(-katakunci) -> judulku2

    judulku2 %>%
      group_by(judul) %>%
      summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                kategori = toString(kategori),
                .groups = "drop") %>%
      select(judul, kategori, kategori2) -> oke

    #hapus duplikat
    judulku2 <- judulku2[!duplicated(judulku2$judul),]

    # berita fixed
    radartuban <- judulku2[,-2]
    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun", judul), -1, 0))
      )
    names(radartuban) <- c("judul", "isiberita", "link", "tanggal", "sentimen", "lokasi", "estimasi lapus")

    radarsemarang <- radartuban
    print(radarsemarang)
    write.xlsx(radarsemarang, paste0("radarsemarang_", Sys.Date(),".xlsx"))
    return(radartuban)
  } else if(wilayahradar == "tulungagung"){
    radartuban <- data.frame()
    for(i in seq(1, x, 1)){
      url <- paste0("https://radartulungagung.jawapos.com/search?q=ekonomi&sort=latest&page=",i)
      laman <- read_html(url)
      judul <- laman %>% html_nodes('.latest__link') %>% html_text()
      tglberita <- laman %>% html_nodes('.latest__date') %>% html_text()
      link_judul <- laman %>% html_nodes('.latest__link') %>% html_attr("href")

      isiberita <- sapply(link_judul, FUN = dptisi, USE.NAMES = F)
      radartuban <- rbind(radartuban, data.frame(judul, tglberita, isiberita, link_judul))
      print(paste("Page ke-", i))
    }

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("Januari", "January", tglberita)) %>%
      mutate(tanggal = gsub("Februari", "February", tanggal)) %>%
      mutate(tanggal = gsub("Maret", "March", tanggal)) %>%
      mutate(tanggal = gsub("Mei", "May", tanggal)) %>%
      mutate(tanggal = gsub("Juni", "June", tanggal)) %>%
      mutate(tanggal = gsub("Juli", "July", tanggal)) %>%
      mutate(tanggal = gsub("Agustus", "August", tanggal)) %>%
      mutate(tanggal = gsub("Oktober", "October", tanggal)) %>%
      mutate(tanggal = gsub("Desember", "December", tanggal))

    radartuban <- radartuban %>%
      mutate(tanggal = gsub("^\\w+, ", "", tanggal),  # Menghapus nama hari
             tanggal = gsub("[|].*", "", tanggal),  # Menghapus bagian setelah tanggal
             tanggal = format(dmy(tanggal), "%Y-%m-%d"))

    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun|Meningkat|Digelontor|Untung|Dorong|Mendorong|Cepat|Pesat|Laris|Optimis|Murah|Mudah|Permudah", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun|Berat|Melorot|Lesuh|Rugi|Buntung|Merugi|Lambat|Lamban|Sulit|Susah|Meleset", judul), -1, 0))
      )

    radartuban <- radartuban %>%
      mutate(lokasi = gsub("\u2013.*|\u2014.*|-.*|, \\d{1,2} \\w+ \\d{4}", "", isiberita)) %>%
      mutate(lokasi = gsub("RADAR TULUNGAGUNG|NASIONAL", "", lokasi)) %>%
      mutate(lokasi = str_squish(lokasi)) %>%
      mutate(lokasi = str_to_title(lokasi))

    radartuban <- radartuban %>%
      filter(grepl("Tulungagung|tulungagung|Trenggalek|trenggalek|Surabaya|surabaya", lokasi))


    radartuban$lokasi <- ifelse(grepl("Tulungagung", radartuban$lokasi, ignore.case = TRUE), "Tulungagung",
                                ifelse(grepl("Trenggalek", radartuban$lokasi, ignore.case = TRUE), "Trenggalek", "Kota Surabaya"))

    #hapus duplikat
    judulku <- radartuban$judul

    #hapus duplikat
    judulku <- radartuban$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas"
    )

    radartuban %>%
      mutate(katakunci = isiberita) %>%
      separate_rows(katakunci, sep = ' ') %>%
      mutate(katakunci = tolower(katakunci)) %>%
      left_join(katelapus) %>%
      filter(!is.na(kategori)) %>%
      select(-katakunci) -> judulku2

    judulku2 %>%
      group_by(judul) %>%
      summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                kategori = toString(kategori),
                .groups = "drop") %>%
      select(judul, kategori, kategori2) -> oke

    #hapus duplikat
    judulku2 <- judulku2[!duplicated(judulku2$judul),]

    # berita fixed
    radartuban <- judulku2[,-2]
    radartuban <- radartuban %>%
      mutate(sentimen = ifelse(grepl("Meroket|Melambung|Melangit|Mencekik|Naik|Bertahan Tinggi|Menggigit|Pedas|Melejit|Merangkak|Membubung|Belum Turun", judul), 1,
                               ifelse(grepl("Turun|Terjun|Menurun|Turbulensi|Nyungsep|Merosot|Menurun", judul), -1, 0))
      )
    names(radartuban) <- c("judul", "isiberita", "link", "tanggal", "sentimen", "lokasi", "estimasi lapus")

    radartulungagung <- radartuban
    print(radartulungagung)
    write.xlsx(radartulungagung, paste0("radartulungagung_", Sys.Date(),".xlsx"))
    return(radartuban)
  } else {
    cat("Maaf, masih terbatas pada wilayah kediri, surabaya, madura, tulungagung, dan semarang\n")
  }
}
