(in-package #:client.core)

(def-view :kit/header (props)
  ((:div class :header)

   ((:a :href "/app")
    "В приложение")

   ((:div class :gap))

   ((:a :href "/about")
    "О системе")

   ((:div class :gap))

   ((:a :href "/manual")
    "Справка")

   ((:div class :gap))

   ((:a :href "/feedback")
    "Обратная связь")))
