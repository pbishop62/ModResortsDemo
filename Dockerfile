FROM quay.io/operator-framework/ansible-operator:latest

# install SSH client
USER root
RUN dnf update -y \ 
    && dnf install -y openssh-clients

COPY ansible.cfg /opt/ansible/.ansible.cfg

RUN chown -R ansible /opt/ansible

USER ansible
ENV ANSIBLE_CONFIG /opt/ansible/.ansible.cfg
ENV ANSIBLE_HOST_KEY_CHECKING=False
