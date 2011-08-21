(defconstants
  (+title+
    "Библиотека Bluetooth_HC05")
  (+top-header+
    "Описание библиотеки Bluetooth_HC05")
  (+synopsis+
    "Библиотека предназначена для работы с Bluetooth-модулем HC-05 и реализует
    все 36 функций, описанных в документации (datasheet) к модулю от компании Wavesen,
    в виде класса Bluetooth_HC05 и его методов.
    Почти все методы принимают в качестве последнего агрумента тайм-аут на чтение ответа
    от модуля (в миллисекундах). К класс Bluetooth_HC05 реализует интерфейс Print
    так что вы можете посылать данные, пользуясь методом print(),
    как в объект класса Serial. В качестве источника информации был использован
    китайский даташит с кучей ошибок и неясностей - такие места я буду обозначать
    специальным значком: ~a. Если вы видите этот значок в описании - значит, я
    ничерта не понял.")

  (+table-of-contents-header+ "Оглавление")

  (+datatypes-header+ "Типы данных")
  (+constants-header+ "Константы")

  (+HC05_DEFAULT_TIMEOUT-description+
    "Тайм-аут по умолчанию для большинства команд.")
  (+HC05_INQUIRY_DEFAULT_TIMEOUT-description+
    "Тайм-аут по умолчанию для опроса (inquiring) Bluetooth-устройств.")
  (+HC05_PAIRING_DEFAULT_TIMEOUT-description+
    "Тайм-аут по умолчанию для образования пары (pairing) между устройствами.")
  (+HC05_PASSWORD_MAXLEN-description+
    "Максимальная длина пароля (PIN-кода), которую позволяет задать модуль.")
  (+HC05_PASSWORD_BUFSIZE-description+
    "Размер буфера, способного вместить самый длинный пароль.")
  (+HC05_NAME_MAXLEN-description+ "Максимальная длина имени устройства.")
  (+HC05_NAME_BUFSIZE-description+ "Размер буфера для имени устройства.")
  (+HC05_ADDRESS_MAXLEN-description+
    "Максимальная длина строкового представления адреса Bluetooth-устройства.")
  (+HC05_ADDRESS_BUFSIZE-description+
    "Размер буфера для строкового представления Bluetooth-адреса.")

  (+HC05_Mode-description+ "Режим работы модуля")
  (+HC05_MODE_DATA-description+ "приём-передача данных")
  (+HC05_MODE_COMMAND-description+ "управление AT-командами")

  (+HC05_Role-description+ "Роль Bluetooth-модуля")
  (+HC05_ROLE_SLAVE-description+ "Пассивное подключение (к модулю можно подключаться)")
  (+HC05_ROLE_MASTER-description+
    "Активное подключение (модуль сам подключается к устройствам)")
  (+HC05_ROLE_SLAVE_LOOP-description+
    "Пассивное подключение с эхом: все принятые данные передаются обратно")

  (+HC05_InquiryMode-description+ "Режим опроса устройств")
  (+HC05_INQUIRY_STANDARD-description+ "Обычный (все устройства)")
  (+HC05_INQUIRY_RSSI-description+ "Только устройства с ~a")
  (+RSSI-wikipedia-link+
    '("RSSI" . "http://ru.wikipedia.org/wiki/Received_Signal_Strength_Indication"))

  (+HC05_Parity-description+ "Использование ~a")
  (+HC05_NO_PARITY-description+ "Без бита чётности")
  (+HC05_PARITY_ODD-description+ "Нечётный")
  (+HC05_PARITY_EVEN-description+ "Чётный")
  (+parity-bit-wikipedia-link+
    '("бита чётности" . "http://ru.wikipedia.org/wiki/Бит_чётности"))

  (+HC05_Connection-description+ "Режим соединения")
  (+bind-method-name+ "bind()")
  (+HC05_CONNECT_BOUND-description+
    "Принимать соединение только от устройства с определённым адресом,
    задаваемым методом ~a")
  (+HC05_CONNECT_ANY-description+ "Принимать соединения от любых устройств")
  (+HC05_CONNECT_SLAVE_LOOP-description+
    "slave-loop?")

  (+security-link+
    '("безопасности" . "http://www.palowireless.com/bluearticles/cc1_security1.asp"))
  (+HC05_Security-description+ "Настройки ~a")
  (+HC05_SEC_OFF-description+ "Выключено?")
  (+HC05_SEC_NON_SECURE-description+ "Незащищённое соединение")
  (+HC05_SEC_SERVICE-description+ "Защита на сервисном уровне (service-level)")
  (+HC05_SEC_LINK-description+ "Защита на уровне соединения (link-level)")
  (+HC05_SEC_UNKNOWN-description+ "Неизвестный режим?")

  (+packet-encryption-section+ "Packet Encryption")
  (+HC05_Encryption-description+
    "Настройки шифрования (см. статью по ~a, раздел ~a)")
  (+HC05_ENC_OFF-description+ "Без шифрования")
  (+HC05_ENC_PTP-description+ "Шифруется только трафик PTP (point-to-point)")
  (+HC05_ENC_PTP_BROADCAST-description+ "Шифруется весь трафик")

  (+HC05_State-description+ "Состояние модуля")
  (+HC05_INITIALIZED-description+ "Инициализирован")
  (+HC05_READY-description+ "Готов. К чему?")
  (+HC05_PAIRABLE-description+ "Готов к образованию пары")
  (+HC05_PAIRED-description+ "Образована пара")
  (+HC05_INQUIRING-description+ "Опрашивает устройства")
  (+HC05_CONNECTING-description+ "Соединяется")
  (+HC05_CONNECTED-description+ "Соединён")
  (+HC05_DISCONNECTED-description+ "Отсоединён")
  (+HC05_UNKNOWN-description+ "Неизвестное состояние?"))
