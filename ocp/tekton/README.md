# OpenShift Pipeline / Tekton

Trigger this against your personal ROKS cluster


## Prerequisites

1. Install the Openshift Pipelines Operator (Pre-installed on OCP 4.7+).
1. Create a new project, `oc login` and `oc project <name>`.
2. Generate a new SSH Key (`ssh-keygen`) and upload `tekton-key.pub` as a Deploy Key in Github Repo (may already be done for CloudBroker-OSDemo repo)

## Setup


1. Create `secret` to store the git SSH private key (uses the `tekton-key` key from the `ssh` folder)

   ```console
   oc create -f secret.yaml
   ```

2. Link the `tekton-key` secret to the `pipeline` (default) service account.

  > **NOTE:** The `pipeline` service account should be visible in every project after the Openshift Pipelines Operator is installed

   ```console
   oc secrets link pipeline tekton-key
   ```

3. Create a `PersisentVolumeClaim`:
   ```console
   oc create -f pvc.yaml
   ```

4. Create the `cics-ansible-runner-task` `Task`:
   ```console
   oc create -f ansible-runner-task.yaml
   ```

5. Create the `cics-pipeline-demo` `Pipeline`:
   ```console
   oc create -f pipeline.yaml
   ```

6. Test the Pipeline manually via the `PipelineRun`:
    ```console
    oc create -f pipelinerun.yaml
    ```

1. Create the `TriggerTemplate`, `TriggerBinding`, and `EventListenter`:
   ```console
   oc create -f trigger.yaml
   ```

1. Create the `Route` to expose the Webhook URL for the created `EventListener`:
    ```console
    oc create -f route.yaml
    ```

