
BUILD_PATH=target/

INSTALL_PATH=/

SCRIPT=/etc/init.d/syncserver

USER=syncserver

USER_SHELL=/bin/bash

build:	clean dirs
	erl -make
	cp -r ebin/* $(BUILD_PATH)/usr/share/syncserver/ebin/
	cp syncserver.app $(BUILD_PATH)/usr/share/syncserver/ebin/
	cp syncserver $(BUILD_PATH)/etc/init.d/
	chmod +x $(BUILD_PATH)/etc/init.d/syncserver
	cp syncserver.config $(BUILD_PATH)/etc/syncserver/

dirs:
	mkdir ebin
	mkdir -p $(BUILD_PATH)/etc/init.d
	mkdir -p $(BUILD_PATH)/etc/syncserver
	mkdir -p $(BUILD_PATH)/usr/share/syncserver/ebin
	mkdir -p $(BUILD_PATH)/var/syncserver
	mkdir -p $(BUILD_PATH)/var/log/syncserver

fullclean:
	rm -rf ebin
	rm -rf $(BUILD_PATH)

clean:
	rm -rf ebin
	rm -rf $(BUILD_PATH)/etc/init.d
	rm -rf $(BUILD_PATH)/usr/

install: build installfiles mkuser
	echo "SyncServer\`s files are installed."
	update-rc.d syncserver defaults
	echo "test.."
	$(SCRIPT) create-cookie || true
	echo "You should create database before starting server"

installfiles: build 
	cp -r $(BUILD_PATH)/* $(INSTALL_PATH)/

uninstall:
	rm -rf $(INSTALL_PATH)/usr/share/syncserver
	rm -rf $(INSTALL_PATH)/etc/syncserver
	rm -f  $(INSTALL_PATH)/etc/init.d/syncserver
	update-rc.d syncserver remove

purge:  uninstall rmuser
	rm -rf $(INSTALL_PATH)/var/syncserver
	rm -rf $(INSTALL_PATH)/var/log/syncserver

mkuser:
	adduser syncserver --group --system \
		--shell $(USER_SHELL) --home /var/syncserver/ --disabled-login
	chown syncserver:syncserver /var/syncserver/
	chmod 770 /var/syncserver/

rmuser:
	deluser syncserver || true



