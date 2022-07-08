# CloudBroker-OSDemo

## Current Status

- OpenShift Pipelines installed and configured on the [`zoscb-acm`](https://console-openshift-console.apps.zoscb-acm.os.fyre.ibm.com/pipelines/ns/ivan-dev/pipeline-runs) cluster.
- IBM Wazi Developer installed and configured on on [`zoscb-acm`](http://codeready-wazi-codeready-workspaces.apps.zoscb-acm.os.fyre.ibm.com) cluster.
- Pipeline can be successfully triggered by a git push to the [IBMZSoftware/CloudBroker-OSDemo](https://github.ibm.com/IBMZSoftware/CloudBroker-OSDemo) repository.

## Running the Playbook

Issue the following command to run the CICS playbook manually:

```console
ansible-playbook -i zsystem.yml playbook.yml
```

## End-to-End Demo

1. Open [Wazi Developer](http://codeready-wazi-codeready-workspaces.apps.zoscb-acm.os.fyre.ibm.com) and navigate to the `CloudBroker-OSDemo` workspace:

1. Edit the file [SDFHSAMP/DFH0XVRC](https://github.ibm.com/IBMZSoftware/CloudBroker-OSDemo/blob/master/SDFHSAMP/DFH0XVRC) and comment out the following lines of code to "fix" the performance issue with the CICS application ([L350-L358](https://github.ibm.com/IBMZSoftware/CloudBroker-OSDemo/blob/master/SDFHSAMP/DFH0XVRC#L350-L358)):

   ```COBOL
   350        MOVE '***TEST***DFH0XVRC****TEST***' TO WS-DESCRIPTION
   351        PERFORM UNTIL DELTA-TIME > 00000050
   352           ACCEPT DAT-TODAY2 FROM DATE
   353           IF DAT-TODAY2 > DAT-TODAY
   354              MOVE 24000000   TO WRK-ONE-DAY
   355           END-IF
   356           ACCEPT TIME-TODAY2 FROM TIME
   357        COMPUTE DELTA-TIME=(TIME-TODAY2 + WRK-ONE-DAY - TIME-TODAY)
   358        END-PERFORM.
   ```

1. Save the file changes.
1. Click the Git source control icon on the left sidebar.
1. Click the `+` icon next to the modified file(s) to stage the changes for commit.
1. Click the `✓` icon to commit the staged file changes.
1. Type a commit message in the popup bar and hit `Enter` to save.
1. Click the `...` icon and select `Push` to push the changes to GitHub.
1. Navigate to [`Pipeline Runs`](https://console-openshift-console.apps.zoscb-acm.os.fyre.ibm.com/pipelines/ns/ivan-dev/pipeline-runs) to monitor the CICS pipeline run execution.
