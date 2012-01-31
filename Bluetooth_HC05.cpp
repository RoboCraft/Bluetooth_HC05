/* Bluetooth_HC05 library implementation.
 * 
 * Copyright (C) 2011 Artem Borisovskiy (bytefu@gmail.com), http://robocraft.ru
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#if defined(ARDUINO) && ARDUINO >= 100
#include "Arduino.h"
#else
#include "WProgram.h"
#endif

#include <avr/pgmspace.h>
#include <ctype.h>
#include <stdio.h>
#include "Bluetooth_HC05.h"


#define PGM_STRING_MAPPED_TO_RAM(name, src) \
  static const char name##_pgm[] PROGMEM = src; \
  char name[sizeof(name##_pgm)]; \
  strcpy_P(name, name##_pgm);


unsigned long htoul(const char *str)
{
  if (!str)
    return 0;

  long result = 0;
  char sign = *str;

  while (*str)
  {
    uint8_t value;

    if (*str >= 'a' && *str <= 'f')
      value = (*str - 'a') + 10;
    else if (*str >= 'A' && *str <= 'F')
      value = (*str - 'A') + 10;
    else if (*str >= '0' && *str <= '9')
      value = *str - '0';
    else
      break;

    result = (result * 16) + value;
    ++str;
  }

  return result;
}


Bluetooth_HC05::Bluetooth_HC05(HardwareSerial &serial):
  m_uart(&serial), m_timeout(HC05_DEFAULT_TIMEOUT), m_ticksAtStart(millis()),
  m_modePin(0xFF), m_resetPin(0xFF), m_errorCode(HC05_OK)
{
}


Bluetooth_HC05::~Bluetooth_HC05()
{
}


HC05_Result Bluetooth_HC05::getLastError() const
{
  return m_errorCode;
}


void Bluetooth_HC05::begin(unsigned baud_rate, uint8_t reset_pin,
  uint8_t mode_pin, HC05_Mode mode)
{
  m_uart->begin(baud_rate);

  m_resetPin = reset_pin;
  pinMode(m_resetPin, OUTPUT);
  digitalWrite(m_resetPin, HIGH);

  m_modePin = mode_pin;
  pinMode(m_modePin, OUTPUT);
  digitalWrite(m_modePin, (mode == HC05_MODE_DATA ? LOW : HIGH));

  hardReset();
}


bool Bluetooth_HC05::probe(unsigned long timeout)
{
  startOperation(timeout);
  writeCommand(0);
  return readOperationResult();
}


void Bluetooth_HC05::hardReset()
{
  digitalWrite(m_resetPin, LOW);
  delay(6);
  digitalWrite(m_resetPin, HIGH);

  m_errorCode = HC05_OK;
}


bool Bluetooth_HC05::softReset(unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(reset_cmd, "RESET");
  return simpleCommand(reset_cmd, 0, timeout);
}


bool Bluetooth_HC05::getVersion(char *buffer, size_t buffer_size, unsigned long timeout)
{
  startOperation(timeout);

  if (!buffer || buffer_size <= 1)
    return false;

  PGM_STRING_MAPPED_TO_RAM(command, "VERSION?");
  writeCommand(command);

  /* Response should look like "+VERSION:2.0-20100601" */
  char response[30];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+VERSION:");
  const char *version = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!version)
  {
    *buffer = 0;
    return readOperationResult() && false;
  }

  snprintf(buffer, buffer_size, "%s", version);
  return readOperationResult();
}


bool Bluetooth_HC05::restoreDefaults(unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command, "ORGL");
  return simpleCommand(command, 0, timeout);
}


bool Bluetooth_HC05::getAddress(BluetoothAddress &address, unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command_name, "ADDR");
  return readAddressWithCommand(address, command_name, timeout);
}


bool Bluetooth_HC05::getName(char *buffer, unsigned long timeout)
{
  startOperation(timeout);

  if (!buffer)
  {
    m_errorCode = HC05_FAIL;
    return false;
  }

  PGM_STRING_MAPPED_TO_RAM(command, "NAME?");
  writeCommand(command);

  char response[40];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+NAME:");
  char *name_part = readResponseWithPrefix(response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!name_part)
  {
    *buffer = 0;
    return readOperationResult() && false;
  }

  PGM_STRING_MAPPED_TO_RAM(format, "%s");
  snprintf(buffer, HC05_NAME_BUFSIZE, format, name_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setName(const char *name, unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command, "NAME=")
  return simpleCommand(command, name, timeout);
}


bool Bluetooth_HC05::getRemoteDeviceName(const BluetoothAddress &address,
  char *buffer, size_t buffer_size, unsigned long timeout)
{
  startOperation(timeout);

  if (!buffer || !buffer_size)
  {
    m_errorCode = HC05_FAIL;
    return false;
  }

  char address_str[HC05_ADDRESS_BUFSIZE];
  printBluetoothAddress(address_str, address, ',');
  PGM_STRING_MAPPED_TO_RAM(command, "RNAME?");
  writeCommand(command, address_str);

  char response[40];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+RNAME:");
  char *name_part = readResponseWithPrefix(response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!name_part)
  {
    *buffer = 0;
    return readOperationResult() && false;
  }

  PGM_STRING_MAPPED_TO_RAM(format, "%s");
  snprintf(buffer, buffer_size, format, name_part);

  return readOperationResult();
}


bool Bluetooth_HC05::getRole(HC05_Role &role, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "ROLE?");
  writeCommand(command);

  char response[20];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+ROLE:");
  const char *role_str = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!role_str)
    return readOperationResult() && false;

  role = static_cast<HC05_Role>(atol(role_str));

  return readOperationResult();
}


bool Bluetooth_HC05::setRole(HC05_Role role, unsigned long timeout)
{
  char role_str[10] = { role, 0 };
  PGM_STRING_MAPPED_TO_RAM(format, "%d");
  snprintf(role_str, sizeof(role_str), format, role);

  PGM_STRING_MAPPED_TO_RAM(command, "ROLE=");
  return simpleCommand(command, role_str, timeout);
}


bool Bluetooth_HC05::getDeviceClass(uint32_t &device_class, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "CLASS?");
  writeCommand(command);

  device_class = 0;

  char response[40];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+CLASS:");
  const char *class_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!class_part)
    return readOperationResult() && false;

  device_class = htoul(class_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setDeviceClass(uint32_t device_class, unsigned long timeout)
{
  char class_str[10];
  PGM_STRING_MAPPED_TO_RAM(format, "%lx");
  snprintf(class_str, sizeof(class_str), format, device_class);

  PGM_STRING_MAPPED_TO_RAM(command, "CLASS=");
  return simpleCommand(command, class_str, timeout);
}


bool Bluetooth_HC05::getInquiryAccessCode(uint32_t &iac, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "IAC?");
  writeCommand(command);

  iac = 0;

  if (isOperationTimedOut())
    return false;

  char response[30];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+IAC:");
  const char *iac_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!iac_part)
    return readOperationResult() && false;

  iac = htoul(iac_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setInquiryAccessCode(uint32_t iac, unsigned long timeout)
{
  char iac_str[10];
  PGM_STRING_MAPPED_TO_RAM(format, "%lx");
  snprintf(iac_str, sizeof(iac_str), format, iac);

  PGM_STRING_MAPPED_TO_RAM(command, "IAC=");
  return simpleCommand(command, iac_str, timeout);
}


bool Bluetooth_HC05::getInquiryMode(HC05_InquiryMode &inq_mode,
  int16_t &max_devices, uint8_t &max_duration, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "INQM?");
  writeCommand(command);

  inq_mode = HC05_INQUIRY_STANDARD;
  max_devices = 0;
  max_duration = 0;

  char response[30];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+INQM:");
  char *mode_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!mode_part)
    return readOperationResult() && false;

  inq_mode = static_cast<HC05_InquiryMode>(atol(mode_part));
  mode_part = strchrnul(mode_part, ',');

  if (*mode_part != ',')
    return readOperationResult() && false;

  max_devices = atol(++mode_part);
  mode_part = strchrnul(mode_part, ',');

  if (*mode_part != ',')
    return readOperationResult() && false;

  max_duration = atol(++mode_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setInquiryMode(HC05_InquiryMode inq_mode,
  int16_t max_devices, uint8_t max_duration, unsigned long timeout)
{
  char mode[20];
  /* Yeah, max_devices is signed 16-bit integer, but the module accepts
   * an unsigned 16-bit integer while actually interpreting it as signed.
   *                          <-^->
   * Tricky chinese engineers (-_-)
   *                            "
   */
  PGM_STRING_MAPPED_TO_RAM(format, "%d,%u,%u");
  snprintf(mode, sizeof(mode), format, inq_mode, (uint16_t)max_devices, max_duration);

  PGM_STRING_MAPPED_TO_RAM(command, "INQM=");
  return simpleCommand(command, mode, timeout);
}


bool Bluetooth_HC05::getPassword(char *buffer, unsigned long timeout)
{
  startOperation(timeout);

  if (!buffer)
  {
    m_errorCode = HC05_FAIL;
    return false;
  }

  PGM_STRING_MAPPED_TO_RAM(command, "PSWD?");
  writeCommand(command);

  char response[HC05_PASSWORD_MAXLEN + 15];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+PSWD:");
  const char *password_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
  {
    *buffer = 0;
    return false;
  }

  if (!password_part)
  {
    *buffer = 0;
    return readOperationResult() && false;
  }

  PGM_STRING_MAPPED_TO_RAM(format, "%s");
  snprintf(buffer, HC05_PASSWORD_BUFSIZE, format, password_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setPassword(const char *password, unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command, "PSWD=");
  return simpleCommand(command, password, timeout);
}


bool Bluetooth_HC05::getSerialMode(uint32_t &speed, uint8_t &stop_bits,
  HC05_Parity &parity, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "UART?");
  writeCommand(command);

  char response[30];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+UART:");
  char *mode_str = readResponseWithPrefix(response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!mode_str)
    return readOperationResult() && false;

  speed = atol(mode_str);
  mode_str = strchrnul(mode_str, ',');

  if (*mode_str != ',')
  {
    m_errorCode = HC05_FAIL;
    return readOperationResult() && false;
  }

  stop_bits = atol(++mode_str) + 1;
  mode_str = strchrnul(mode_str, ',');

  if (*mode_str != ',')
  {
    m_errorCode = HC05_FAIL;
    return readOperationResult() && false;
  }

  parity = static_cast<HC05_Parity>(atol(++mode_str));

  return readOperationResult();
}


bool Bluetooth_HC05::setSerialMode(uint32_t speed,
  uint8_t stop_bits, HC05_Parity parity, unsigned long timeout)
{
  stop_bits -= 1; // 0: 1 stop bit, 1: 2 stop bits, any other are not allowed

  char mode_str[20];
  PGM_STRING_MAPPED_TO_RAM(format, "%lu,%u,%u");
  snprintf(mode_str, sizeof(mode_str), format, speed, stop_bits, parity);

  PGM_STRING_MAPPED_TO_RAM(command, "UART=");
  return simpleCommand(command, mode_str, timeout);
}


bool Bluetooth_HC05::getConnectionMode(
  HC05_Connection &connection_mode, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "CMODE?");
  writeCommand(command);

  char response[20];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+CMOD:");
  const char *mode_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!mode_part)
    return readOperationResult() && false;

  connection_mode = static_cast<HC05_Connection>(atol(mode_part));

  return readOperationResult();
}


bool Bluetooth_HC05::setConnectionMode(
  HC05_Connection connection_mode, unsigned long timeout)
{
  char mode_str[20];
  PGM_STRING_MAPPED_TO_RAM(format, "%u");
  snprintf(mode_str, sizeof(mode_str), format, connection_mode);

  PGM_STRING_MAPPED_TO_RAM(command, "CMODE=");
  return simpleCommand(command, mode_str, timeout);
}


bool Bluetooth_HC05::bind(const BluetoothAddress &address, unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command_name, "BIND");
  return writeAddressWithCommand(address, command_name, timeout);
}


bool Bluetooth_HC05::getAddressBound(BluetoothAddress &address, unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command_name, "BIND");
  return readAddressWithCommand(address, "BIND", timeout);
}


bool Bluetooth_HC05::getLeds(bool &led_status,
  bool &led_connection, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "POLAR?");
  writeCommand(command);

  led_status = 0;
  led_connection = 0;

  char response[30];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+POLAR:");
  char *status_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!status_part)
    return readOperationResult() && false;

  led_status = atol(status_part);
  status_part = strchrnul(status_part, ',');

  if (*status_part != ',')
    return readOperationResult() && false;

  led_connection = atol(++status_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setLeds(bool led_status,
  bool led_connection, unsigned long timeout)
{
  char leds_str[10];
  PGM_STRING_MAPPED_TO_RAM(format, "%d,%d");
  snprintf(leds_str, sizeof(leds_str), format,
    (led_status ? 1 : 0), (led_connection ? 1 : 0));

  PGM_STRING_MAPPED_TO_RAM(command, "POLAR=");
  return simpleCommand(command, leds_str, timeout);
}


bool Bluetooth_HC05::setPortState(uint8_t port_num,
  uint8_t port_state, unsigned long timeout)
{
  char state_str[10];
  PGM_STRING_MAPPED_TO_RAM(format, "%u,%u");
  snprintf(state_str, sizeof(state_str), format, port_num, port_state);

  PGM_STRING_MAPPED_TO_RAM(command, "PIO=");
  return simpleCommand(command, state_str, timeout);
}


bool Bluetooth_HC05::getMultiplePorts(uint16_t &port_states, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "MPIO?");
  writeCommand(command);

  port_states = 0;

  char response[20];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+MPIO:");
  const char *states_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!states_part)
    return readOperationResult() && false;

  port_states = htoul(states_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setMultiplePorts(uint16_t port_states, unsigned long timeout)
{
  char states_str[10];
  PGM_STRING_MAPPED_TO_RAM(format, "%x");
  snprintf(states_str, sizeof(states_str), format, port_states);

  PGM_STRING_MAPPED_TO_RAM(command, "MPIO=");
  return simpleCommand(command, states_str, timeout);
}


bool Bluetooth_HC05::getInquiryAndPagingParams(
  uint16_t &inquiry_interval, uint16_t &inquiry_duration,
  uint16_t &paging_interval, uint16_t &paging_duration, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "IPSCAN?");
  writeCommand(command);

  char response[40];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+IPSCAN:");
  char *params_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!params_part)
    return readOperationResult() && false;

  inquiry_interval = atol(params_part);
  params_part = strchrnul(params_part, ',');

  if (*params_part != ',')
    return readOperationResult() && false;

  inquiry_duration = atol(++params_part);
  params_part = strchrnul(params_part, ',');

  if (*params_part != ',')
    return readOperationResult() && false;

  paging_interval = atol(++params_part);
  params_part = strchrnul(params_part, ',');

  if (*params_part != ',')
    return readOperationResult() && false;

  paging_duration = atol(++params_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setInquiryAndPagingParams(
  uint16_t inquiry_interval, uint16_t inquiry_duration,
  uint16_t paging_interval, uint16_t paging_duration, unsigned long timeout)
{
  char params_str[40];
  PGM_STRING_MAPPED_TO_RAM(format, "%u,%u,%u,%u");
  snprintf(params_str, sizeof(params_str), format,
    inquiry_interval, inquiry_duration, paging_interval, paging_duration);

  PGM_STRING_MAPPED_TO_RAM(command, "IPSCAN=");
  return simpleCommand(command, params_str, timeout);
}


bool Bluetooth_HC05::getSniffParams(uint16_t &max_time, uint16_t &min_time,
  uint16_t &retry_interval, uint16_t &sniff_timeout, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "SNIFF?");
  writeCommand(command);

  char response[40];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+SNIFF:");
  char *params_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!params_part)
    return readOperationResult() && false;

  max_time = atol(params_part);
  params_part = strchrnul(params_part, ',');

  if (*params_part != ',')
    return readOperationResult() && false;

  min_time = atol(++params_part);
  params_part = strchrnul(params_part, ',');

  if (*params_part != ',')
    return readOperationResult() && false;

  retry_interval = atol(++params_part);
  params_part = strchrnul(params_part, ',');

  if (*params_part != ',')
    return readOperationResult() && false;

  sniff_timeout = atol(++params_part);

  return readOperationResult();
}


bool Bluetooth_HC05::setSniffParams(uint16_t max_time, uint16_t min_time,
  uint16_t retry_interval, uint16_t sniff_timeout, unsigned long timeout)
{
  char params_str[40];
  PGM_STRING_MAPPED_TO_RAM(format, "%u,%u,%u,%u");
  snprintf(params_str, sizeof(params_str), format,
    max_time, min_time, retry_interval, sniff_timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "SNIFF=");
  return simpleCommand(command, params_str, timeout);
}


bool Bluetooth_HC05::enterSniffMode(unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command, "ENSNIFF");
  return simpleCommand(command, 0, timeout);
}


bool Bluetooth_HC05::exitSniffMode(unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command, "EXSNIFF");
  return simpleCommand(command, 0, timeout);
}


bool Bluetooth_HC05::getSecurityAndEncryption(HC05_Security &security,
  HC05_Encryption &encryption, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "SENM?");
  writeCommand(command);

  char response[20];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+SENM:");
  char *params_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!params_part)
    return readOperationResult() && false;

  security = static_cast<HC05_Security>(atol(params_part));
  params_part = strchrnul(params_part, ',');

  if (*params_part != ',')
    return readOperationResult() && false;

  encryption = static_cast<HC05_Encryption>(atol(++params_part));

  return readOperationResult();
}


bool Bluetooth_HC05::setSecurityAndEncryption(HC05_Security security,
  HC05_Encryption encryption, unsigned long timeout)
{
  char params_str[10];
  PGM_STRING_MAPPED_TO_RAM(format, "%u,%u");
  snprintf(params_str, sizeof(params_str), format, security, encryption);

  PGM_STRING_MAPPED_TO_RAM(command, "SENM=");
  return simpleCommand(command, params_str, timeout);
}


bool Bluetooth_HC05::deleteDeviceFromList(
    const BluetoothAddress &address, unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command_name, "RMSAD");
  return writeAddressWithCommand(address, command_name, timeout);
}


bool Bluetooth_HC05::deleteAllDevicesFromList(unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command, "RMAAD");
  return simpleCommand(command, 0, timeout);
}


bool Bluetooth_HC05::findDeviceInList(
  const BluetoothAddress &address, unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command_name, "FSAD");
  return writeAddressWithCommand(address, command_name, timeout);
}


bool Bluetooth_HC05::countDevicesInList(uint8_t &device_count, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "ADCN?");
  writeCommand(command);

  char response[20];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+ADCN:");
  const char *count_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!count_part)
    return readOperationResult() && false;

  device_count = atol(count_part);

  return readOperationResult();
}


bool Bluetooth_HC05::getLastAuthenticatedDevice(
  BluetoothAddress &address, unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command_name, "MRAD");
  return readAddressWithCommand(address, command_name, timeout);
}


bool Bluetooth_HC05::getState(HC05_State &state, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "STATE?");
  writeCommand(command);

  state = HC05_UNKNOWN;

  char response[40];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+STATE:");
  const char *status_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!status_part)
    return readOperationResult() && false;

  PGM_STRING_MAPPED_TO_RAM(INITIALIZED, "INITIALIZED");
  PGM_STRING_MAPPED_TO_RAM(READY, "READY");
  PGM_STRING_MAPPED_TO_RAM(PAIRABLE, "PAIRABLE");
  PGM_STRING_MAPPED_TO_RAM(PAIRED, "PAIRED");
  PGM_STRING_MAPPED_TO_RAM(INQUIRING, "INQUIRING");
  PGM_STRING_MAPPED_TO_RAM(CONNECTING, "CONNECTING");
  PGM_STRING_MAPPED_TO_RAM(CONNECTED, "CONNECTED");
  PGM_STRING_MAPPED_TO_RAM(DISCONNECTED, "DISCONNECTED");
  PGM_STRING_MAPPED_TO_RAM(UNKNOWN, "UNKNOWN");

  if (strcmp(status_part, INITIALIZED) == 0)
    state = HC05_INITIALIZED;
  else if (strcmp(status_part, READY) == 0)
    state = HC05_READY;
  else if (strcmp(status_part, PAIRABLE) == 0)
    state = HC05_PAIRABLE;
  else if (strcmp(status_part, PAIRED) == 0)
    state = HC05_PAIRED;
  else if (strcmp(status_part, INQUIRING) == 0)
    state = HC05_INQUIRING;
  else if (strcmp(status_part, CONNECTING) == 0)
    state = HC05_CONNECTING;
  else if (strcmp(status_part, CONNECTED) == 0)
    state = HC05_CONNECTED;
  else if (strcmp(status_part, DISCONNECTED) == 0)
    state = HC05_DISCONNECTED;
  else if (strcmp(status_part, UNKNOWN) == 0)
    state = HC05_UNKNOWN;

  return readOperationResult();
}


bool Bluetooth_HC05::initSerialPortProfile(unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command, "INIT");
  return simpleCommand(command, 0, timeout);
}


bool Bluetooth_HC05::inquire(InquiryCallback callback, unsigned long timeout)
{
  startOperation(timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "INQ");
  writeCommand(command);

  while (!isOperationTimedOut())
  {
    if (m_uart->peek() != '+')
      break;

    char response[HC05_ADDRESS_BUFSIZE + 10];
    PGM_STRING_MAPPED_TO_RAM(response_pattern, "+INQ:");
    const char *address_part;
    address_part = readResponseWithPrefix(response, sizeof(response), response_pattern);

    BluetoothAddress address;
    parseBluetoothAddress(address, address_part, ':');

    if (callback)
      callback(address);
  }

  return readOperationResult();
}


bool Bluetooth_HC05::cancelInquiry(unsigned long timeout)
{
  PGM_STRING_MAPPED_TO_RAM(command, "INQC");
  return simpleCommand(command, 0, timeout);
}


bool Bluetooth_HC05::pair(const BluetoothAddress &address, unsigned long timeout)
{
  char params_str[HC05_ADDRESS_BUFSIZE + 15];
  int address_length = printBluetoothAddress(params_str, address, ',');

  PGM_STRING_MAPPED_TO_RAM(format, ",%lu");
  snprintf(params_str + address_length,
    sizeof(params_str) - address_length, format, timeout);

  PGM_STRING_MAPPED_TO_RAM(command, "PAIR");
  return simpleCommand(command, params_str, timeout);
}


bool Bluetooth_HC05::connect(const BluetoothAddress &address, unsigned long timeout)
{
  return writeAddressWithCommand(address, "LINK", timeout);
}


bool Bluetooth_HC05::disconnect(unsigned long timeout)
{
  startOperation(timeout);
  writeCommand("DISC");

  PGM_STRING_MAPPED_TO_RAM(SUCCESS, "SUCCESS");
  PGM_STRING_MAPPED_TO_RAM(LINK_LOSS, "LINK_LOSS");
  PGM_STRING_MAPPED_TO_RAM(NO_SLC, "NO_SLC");
  PGM_STRING_MAPPED_TO_RAM(TIMEOUT, "TIMEOUT");
  PGM_STRING_MAPPED_TO_RAM(ERROR, "ERROR");

  char response[20];
  PGM_STRING_MAPPED_TO_RAM(response_pattern, "+DISC:");
  const char *status_part = readResponseWithPrefix(
    response, sizeof(response), response_pattern);

  if (strcmp(status_part, SUCCESS) == 0)
    m_errorCode = HC05_OK;
  else if (strcmp(status_part, LINK_LOSS) == 0)
    m_errorCode = HC05_ERR_DISC_LINK_LOSS;
  else if (strcmp(status_part, NO_SLC) == 0)
    m_errorCode = HC05_ERR_DISC_NO_SLC;
  else if (strcmp(status_part, TIMEOUT) == 0)
    m_errorCode = HC05_ERR_DISC_TIMEOUT;
  else if (strcmp(status_part, ERROR) == 0)
    m_errorCode = HC05_ERR_DISC_ERROR;

   return readOperationResult();
}


bool Bluetooth_HC05::readAddressWithCommand(BluetoothAddress &address,
  const char *command_name, unsigned long timeout)
{
  startOperation(timeout);
  memset(address, 0, sizeof(BluetoothAddress));

  if (!command_name)
    return false;

  writeCommand(command_name, "?");

  /* Response should look like "+<command_name>:<NAP>:<UAP>:<LAP>",
   * where actual address will look like "1234:56:abcdef".
   */
  char response[HC05_ADDRESS_BUFSIZE + 20];
  char response_pattern[20];

  PGM_STRING_MAPPED_TO_RAM(format, "+%s:");
  snprintf(response_pattern, sizeof(response_pattern), format, command_name);

  char *address_part = readResponseWithPrefix(response, sizeof(response), response_pattern);

  if (m_errorCode != HC05_OK)
    return false;

  if (!address_part)
    return readOperationResult() && false;

  if (!parseBluetoothAddress(address, address_part, ':'))
    return readOperationResult() && false;

  return readOperationResult();
}


bool Bluetooth_HC05::writeAddressWithCommand(const BluetoothAddress &address,
  const char *command_name, unsigned long timeout)
{
  char command[20];
  PGM_STRING_MAPPED_TO_RAM(format, "%s=");
  snprintf(command, sizeof(command), format, command_name);

  char address_str[HC05_ADDRESS_BUFSIZE];
  printBluetoothAddress(address_str, address, ',');

  return simpleCommand(command, address_str, timeout);
}


bool Bluetooth_HC05::simpleCommand(
  const char *command, const char *arg, unsigned long timeout)
{
  startOperation(timeout);
  writeCommand(command, arg);
  return readOperationResult();
}


bool Bluetooth_HC05::readOperationResult()
{
  char response[15];
  readLine(response, sizeof(response));

  PGM_STRING_MAPPED_TO_RAM(OK, "OK");
  return strcmp(response, OK) == 0;
}


void Bluetooth_HC05::writeCommand(const char *command, const char *arg)
{
  PGM_STRING_MAPPED_TO_RAM(AT, "AT");
  m_uart->print(AT);

  if (command && command[0] != 0)
  {
    m_uart->write('+');
    m_uart->print(command);
  }

  if (arg && arg[0] != 0)
    m_uart->print(arg);

  PGM_STRING_MAPPED_TO_RAM(EOL, "\r\n");
  m_uart->print(EOL);
}


size_t Bluetooth_HC05::readLine(char *buffer, size_t buffer_size)
{
  if (!buffer || buffer_size <= 1)
    return 0;

  char *p = buffer;
  *p = 0;

  while (((size_t)(p - buffer) < buffer_size - 1) && (p[-1] != '\n' || p[-2] != '\r'))
  {
    while (!m_uart->available())
    {
      if (isOperationTimedOut())
        goto EXIT_LOOP;
    }

    *p++ = m_uart->read();
    continue;

  EXIT_LOOP:
    break;
  }

  if (p[-1] == '\n' && p[-2] == '\r')
    p -= 2;

  *p = 0;

  PGM_STRING_MAPPED_TO_RAM(error_prefix, "ERROR:(");

  if (char *error_code_str = skipPrefix(buffer, buffer_size, error_prefix))
    m_errorCode = static_cast<HC05_Result>(htoul(error_code_str));
  else if (strcmp(buffer, "FAIL") == 0)
    m_errorCode = HC05_FAIL;

  size_t num_bytes = p - buffer;
  return num_bytes;
}


bool Bluetooth_HC05::parseBluetoothAddress(
  BluetoothAddress &address, const char *address_str, char delimiter)
{
  /* Address should look like "+ADDR:<NAP>:<UAP>:<LAP>",
   * where actual address will look like "1234:56:abcdef".
   */
  if (!address || !address_str)
    return false;

  char *digits_ptr = const_cast<char*>(address_str);
  uint8_t NAP[2];
  *((uint16_t*)NAP) = htoul(digits_ptr);
  digits_ptr = strchrnul(digits_ptr, delimiter);

  if (*digits_ptr != delimiter)
    return false;

  uint8_t UAP = htoul(++digits_ptr);
  digits_ptr = strchrnul(digits_ptr, delimiter);

  if (*digits_ptr != delimiter)
    return false;

  uint8_t LAP[4];
  *((uint32_t*)LAP) = htoul(++digits_ptr);

  address[0] = NAP[1];
  address[1] = NAP[0];
  address[2] = UAP;
  address[3] = LAP[2];
  address[4] = LAP[1];
  address[5] = LAP[0];

  return true;
}


int Bluetooth_HC05::printBluetoothAddress(char *address_str,
  const BluetoothAddress &address, char delimiter)
{
  if (!address || !address_str)
    return 0;

  uint8_t NAP[2];
  NAP[0] = address[1];
  NAP[1] = address[0];

  uint8_t UAP = address[2];

  uint8_t LAP[4];
  LAP[0] = address[5];
  LAP[1] = address[4];
  LAP[2] = address[3];
  LAP[3] = 0;

  PGM_STRING_MAPPED_TO_RAM(format, "%x%c%x%c%lx");

  int written = snprintf(address_str, HC05_ADDRESS_BUFSIZE, format,
    *reinterpret_cast<const uint16_t*>(NAP),
    delimiter, UAP, delimiter,
    *reinterpret_cast<const uint32_t*>(LAP));

  return written;
}


char *Bluetooth_HC05::readResponseWithPrefix(
  char *buffer, size_t buffer_size, const char *prefix)
{
  if (!buffer || buffer_size <= 1)
    return 0;

  size_t response_length = readLine(buffer, buffer_size);
  char *postfix = skipPrefix(buffer, response_length, prefix);

  if (!postfix)
    *buffer = 0;

  return postfix;
}


char *Bluetooth_HC05::skipPrefix(char *str, size_t str_length, const char *prefix)
{
  if (!str || str_length == 0 || !prefix)
    return 0;

  size_t prefix_length = strlen(prefix);

  if (str_length >= prefix_length && strncmp(str, prefix, prefix_length) == 0)
    return str + prefix_length;

  return 0;
}


void Bluetooth_HC05::startOperation(unsigned long timeout)
{
  m_ticksAtStart = millis();
  m_timeout = timeout;
  m_errorCode = HC05_OK;
}


bool Bluetooth_HC05::isOperationTimedOut() const
{
  return operationDuration() >= m_timeout;
}


unsigned long Bluetooth_HC05::operationDuration() const
{
  unsigned long current_ticks = millis(),
                elapsed_ticks;

  if (current_ticks >= m_ticksAtStart)
    elapsed_ticks = current_ticks - m_ticksAtStart;
  else
    elapsed_ticks = (ULONG_MAX - m_ticksAtStart) + current_ticks;

  return elapsed_ticks;
}

#if defined(ARDUINO) && ARDUINO >= 100
size_t Bluetooth_HC05::write(uint8_t data)
#else
void Bluetooth_HC05::write(uint8_t data)
#endif
{
  #if defined(ARDUINO) && ARDUINO >= 100
  return
  #endif
  m_uart->write(data);
}
