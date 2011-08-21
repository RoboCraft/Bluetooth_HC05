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
    ничерта не понял из документации и объяснить не могу.")

  (+table-of-contents-header+ "Оглавление")

  (+types-header+ "Типы")
  (+constants-header+ "Константы")

  (HC05_DEFAULT_TIMEOUT
    "Тайм-аут по умолчанию для большинства команд.")
  (HC05_INQUIRY_DEFAULT_TIMEOUT
    "Тайм-аут по умолчанию для опроса (inquiring) Bluetooth-устройств.")
  (HC05_PAIRING_DEFAULT_TIMEOUT
    "Тайм-аут по умолчанию для образования пары (pairing) между устройствами.")
  (HC05_PASSWORD_MAXLEN
    "Максимальная длина пароля (PIN-кода), которую позволяет задать модуль.")
  (HC05_PASSWORD_BUFSIZE
    "Размер буфера, способного вместить самый длинный пароль.")
  (HC05_NAME_MAXLEN "Максимальная длина имени устройства.")
  (HC05_NAME_BUFSIZE "Размер буфера для имени устройства.")
  (HC05_ADDRESS_MAXLEN
    "Максимальная длина строкового представления адреса Bluetooth-устройства.")
  (HC05_ADDRESS_BUFSIZE
    "Размер буфера для строкового представления Bluetooth-адреса.")

  (+BluetoothAddress+ "Адрес Bluetooth-устройства, 6-байтовый массив")

  (HC05_Mode "Режим работы модуля")
  (HC05_MODE_DATA "приём-передача данных")
  (HC05_MODE_COMMAND "управление AT-командами")

  (HC05_Role "Роль Bluetooth-модуля")
  (HC05_ROLE_SLAVE "Пассивное подключение (к модулю можно подключаться)")
  (HC05_ROLE_MASTER
    "Активное подключение (модуль сам подключается к устройствам)")
  (HC05_ROLE_SLAVE_LOOP
    "Пассивное подключение с эхом: все принятые данные передаются обратно")

  (HC05_InquiryMode "Режим опроса устройств")
  (HC05_INQUIRY_STANDARD "Обычный (все устройства)")
  (HC05_INQUIRY_RSSI "Только устройства с ~a")
  (+RSSI-wikipedia-link+
    '("RSSI" . "http://ru.wikipedia.org/wiki/Received_Signal_Strength_Indication"))

  (HC05_Parity "Использование ~a")
  (HC05_NO_PARITY "Без бита чётности")
  (HC05_PARITY_ODD "Нечётный")
  (HC05_PARITY_EVEN "Чётный")
  (+parity-bit-wikipedia-link+
    '("бита чётности" . "http://ru.wikipedia.org/wiki/Бит_чётности"))

  (HC05_Connection "Режим соединения")
  (HC05_CONNECT_BOUND "Принимать соединение только от устройства
    с определённым адресом, задаваемым методом ")
  (HC05_CONNECT_ANY "Принимать соединения от любых устройств")
  (HC05_CONNECT_SLAVE_LOOP
    "slave-loop?")

  (+security-link-name+ "безопасности")
  (HC05_Security "Настройки ~a")
  (HC05_SEC_OFF "Выключено?")
  (HC05_SEC_NON_SECURE "Незащищённое соединение")
  (HC05_SEC_SERVICE "Защита на сервисном уровне (service-level)")
  (HC05_SEC_LINK "Защита на уровне соединения (link-level)")
  (HC05_SEC_UNKNOWN "Неизвестный режим?")

  (+packet-encryption-section+ "Packet Encryption")
  (HC05_Encryption
    "Настройки шифрования (см. статью по ~a, раздел ~a)")
  (HC05_ENC_OFF "Без шифрования")
  (HC05_ENC_PTP "Шифруется только трафик PTP (point-to-point)")
  (HC05_ENC_PTP_BROADCAST "Шифруется весь трафик")

  (HC05_State "Состояние модуля")
  (HC05_INITIALIZED "Инициализирован")
  (HC05_READY "Готов. К чему?")
  (HC05_PAIRABLE "Готов к образованию пары")
  (HC05_PAIRED "Образована пара")
  (HC05_INQUIRING "Опрашивает устройства")
  (HC05_CONNECTING "Соединяется")
  (HC05_CONNECTED "Соединён")
  (HC05_DISCONNECTED "Отсоединён")
  (HC05_UNKNOWN "Неизвестное состояние?")

  (HC05_Result
    "Результат последней выполненной команды")
  (HC05_OK "Команда выполнена успешно")
  (HC05_FAIL "Ошибка выполнения, причина неизвестна")
  (HC05_ERR_TIMEOUT "Вышло время ожидания ответа от модуля")
  (HC05_ERR_ARGUMENT "Методу переданы неверные аргументы")
  (HC05_ERR_DISC_LINK_LOSS "Связь с устройсвом потеряна")
  (HC05_ERR_DISC_NO_SLC "")
  (HC05_ERR_DISC_TIMEOUT "Тайм-аут отсоединения?")
  (HC05_ERR_DISC_ERROR "Ошибка отсоединения?")
  (HC05_ERR_AT_COMMAND "Ошибка синтаксиса AT-команды")
  (HC05_ERR_DEFAULT_RESULT "Результат по умолчанию?")
  (HC05_ERR_PSKEY_WRITE "Ошибка сохранения пароля")
  (HC05_ERR_DEVICE_NAME_TOO_LONG "Слишком длиное имя устройства")
  (HC05_ERR_NO_DEVICE_NAME "Имя устройства не указано")
  (HC05_ERR_NAP_TOO_LONG "Часть адреса NAP слишком длинная")
  (HC05_ERR_UAP_TOO_LONG "Часть адреса UAP слишком длинная")
  (HC05_ERR_LAP_TOO_LONG "Часть адреса LAP слишком длинная")
  (+setMultiplePorts-method-name+ "setMultiplePorts()")
  (HC05_ERR_NO_PIO_MASK "Не указана маска портов")
  (+setPortState-method-name+ "setPortState()")
  (HC05_ERR_NO_PIO_NUMBER "Не указан номер порта")
  (+setDeviceClass-method-name+ "setDeviceClass()")
  (HC05_ERR_NO_DEVICE_TYPE "Не указан класс устройства")
  (HC05_ERR_DEVICE_TYPE_TOO_LONG
    "Слишком длинное значение класса устройства")
  (+setInquiryAccessCode-method-name+ "setInquiryAccessCode()")
  (HC05_ERR_NO_IAC "Пустой код опроса IAC (Inquire Access Code)")
  (HC05_ERR_IAC_TOO_LONG "Слишком длинный IAC")
  (HC05_ERR_INVALID_IAC "Недопустимый IAC")
  (HC05_ERR_NO_PASSWORD "Пароль пуст")
  (HC05_ERR_PASSWORD_TOO_LONG "Слишком длинный пароль")
  (HC05_ERR_INVALID_MODULE_ROLE "Неверная роль устройства")
  (HC05_ERR_INVALID_BAUD_RATE "Недопустимая скорость передачи")
  (HC05_ERR_INVALID_STOP_BITS "Недопустимая настройка стоп-битов")
  (HC05_ERR_INVALID_PARITY_BITS "Недопустимая настройка бита чётности")
  (HC05_ERR_DEVICE_NOT_IN_LIST "Устройство не в списке авторизованных")
  (HC05_ERR_SPP_NOT_INITIALIZED
    "Профиль последовательного порта (SPP, Serial Port Profile) не инициализирован")
  (HC05_ERR_SPP_REINIT "Повторная инициализация SPP")
  (HC05_ERR_INVALID_INQUIRY_MODE "Недопустимый режим опроса")
  (HC05_ERR_INQUIRY_TIMEOUT_TOO_LONG "Слишком большой тайм-аут опроса")
  (HC05_ERR_NO_BLUETOOTH_ADDRESS "Не указан Bluetooth-адрес")
  (HC05_ERR_SECURITY_MODE "Недопустимый режим безопасности")
  (HC05_ERR_ENCRYPTION_MODE "Недопустимый режим шифрования")
  )
