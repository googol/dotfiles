ANSIBLE_VENV_DIR=.temp/ansible.venv
ANSIBLE_BIN=$(ANSIBLE_VENV_DIR)/bin/ansible

$(ANSIBLE_BIN): scripts/setup-venv ansible-requirements/* ansible-requirements/packages/*
	@scripts/setup-venv "$(ANSIBLE_VENV_DIR)"
	@touch "$(ANSIBLE_BIN)"

.PHONY: setup
setup: $(ANSIBLE_BIN)

.PHONY: clean
clean:
	rm -rf .temp
