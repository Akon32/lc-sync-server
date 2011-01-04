
BUILD_PATH=target/

INSTALL_PATH=/

SCRIPT=/etc/init.d/syncserver

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

install: build installfiles
	echo "SyncServer\`s files are installed."
	echo "test.."
	$(SCRIPT)
	echo "You may create database and cookie file before starting server"

installfiles: build
	cp -r $(BUILD_PATH)/* $(INSTALL_PATH)/

uninstall:
	rm -rf $(INSTALL_PATH)/usr/share/syncserver
	rm -rf $(INSTALL_PATH)/etc/syncserver
	rm -f  $(INSTALL_PATH)/etc/init.d/syncserver

purge:  uninstall
	rm -rf $(INSTALL_PATH)/var/syncserver
	rm -rf $(INSTALL_PATH)/var/log/syncserver

