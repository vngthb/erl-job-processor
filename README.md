# erl-job-processor

Developed on an Ubuntu subsystem with Erlang/OTP 24 and Rebar 3.18.0

After clonning the repository:
  1. open the OS command line interface;
  2. change the current directory to /erl-job-processor (__cd */erl-job-processor__);
  3. run app through rebar shell (**rebar3 shell --config config/sys.config**).

 > Note: The default port is 8082, it can by changed within __sys.config__ (*/erl-job-processorconfig/sys.config).

Once the app started use a framework for testing APIs (e.g. Postman or Thunder Client for Visual Code) to send requests to available endpoints:

  1.  method: **POST**, endpoint: **:8082/sort-job-tasks** - returns the same job with ordered tasks based on their requirements or an error;
  2.  method: **POST**, endpoint: **:8082/job-to-bash** - returns a bash script representation of the job or an error.

Or use **curl** from another command line interface (make sure you're in the project directory to be able to access the test data):

    1. endpoint  :8082/job-to-bash:
    - curl -d "@testdata/for-valid-response.json" -X POST http://localhost:8082/job-to-bash -w "\n"
    - curl -d "@testdata/for-cyclic-requirements.json" -X POST http://localhost:8082/job-to-bash -w "\n"
    - curl -d "@testdata/for-missing-requirements.json" -X POST http://localhost:8082/job-to-bash -w "\n"

    2. endpoint :8082/sort-job-tasks:
    - curl -d "@testdata/for-valid-response.json" -X POST http://localhost:8082/sort-job-tasks -w "\n"
    - curl -d "@testdata/for-cyclic-requirements.json" -X POST http://localhost:8082/sort-job-tasks -w "\n"
    - curl -d "@testdata/for-missing-requirements.json" -X POST http://localhost:8082/sort-job-tasks -w "\n"

Payload examples:
<details>
<summary>1. for a valid response</summary>
  
```
{
    "tasks": [
        {
            "name": "task-1",
            "command": "touch /tmp/file1"
        },
        {
            "name": "task-2",
            "command": "cat /tmp/file1",
            "requires": [
                "task-3"
            ]
        },
        {
            "name": "task-3",
            "command": "echo 'Hello World!' > /tmp/file1",
            "requires": [
                "task-1"
            ]
        },
        {
            "name": "task-4",
            "command": "rm /tmp/file1",
            "requires": [
                "task-2",
                "task-3"
            ]
        }
    ]
}
```
  
</details>

<details>
<summary>2. for a bad request, reason: cyclic_requirements</summary>
  
```
{
    "tasks": [
        {
            "name": "task-1",
            "command": "touch /tmp/file1"
        },
        {
            "name": "task-2",
            "command": "cat /tmp/file1",
            "requires": [
                "task-3"
            ]
        },
        {
            "name": "task-3",
            "command": "echo 'Hello World!' > /tmp/file1",
            "requires": [
                "task-2"
            ]
        },
        {
            "name": "task-4",
            "command": "rm /tmp/file1",
            "requires": [
                "task-2",
                "task-3"
            ]
        }
    ]
}
```

</details>

<details>
<summary>3. for a bad request, reason: missing_requirements</summary>
  
```
{
    "tasks": [
        {
            "name": "task-1",
            "command": "touch /tmp/file1"
        },
        {
            "name": "task-2",
            "command": "cat /tmp/file1",
            "requires": [
                "task-7"
            ]
        },
        {
            "name": "task-3",
            "command": "echo 'Hello World!' > /tmp/file1",
            "requires": [
                "task-1"
            ]
        },
        {
            "name": "task-4",
            "command": "rm /tmp/file1",
            "requires": [
                "task-2",
                "task-3"
            ]
        }
    ]
}
```

</details>
