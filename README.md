# erl-job-processor

Developed on an Ubuntu subsystem with Erlang/OTP 24 and Rebar 3.18.0

After clonning the repository:
  1. open the OS command line interface;
  2. change the current directory to /erl-job-processor (__cd */erl-job-processor__);
  3. run app through rebar shell (**rebar3 shell --config config/sys.config**).

 > Note: The default port is 8082, it can by changed within __sys.config__ (*/erl-job-processorconfig/sys.config).

Once the app started use a framework for testing APIs (e.g. Postman or Thunder Client for Visual Code) to send requests to available endpoints:
  1.  method: **POST**, endpoint: **:8082/sort-job-tasks**
  2.  method: **POST**, endpoint: **:8082/job-to-bash**

Payload examples:
<details>
<summary>1. valid response</summary>
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
<summary>2. bad request, reason: cyclic_requirements</summary>
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
<summary>3. bad request, reason: missing_requirements</summary>
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
