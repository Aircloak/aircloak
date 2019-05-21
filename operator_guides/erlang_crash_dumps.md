By default, Erlang crash dumps in release images are not generated. Such configuration prevents possible information leaks, and reduces storage requirements of the system.

However, in some cases the Aircloak support might need to collect and analyze a crash dump to understand the problem. This procedure has to be performed in coordination with the operators of the Aircloak system. The operator must be informed that a crash dump may contain sensitive information. Therefore, it's advised that the crash dump file is never shared with anyone (not even with the Aircloak support), and that it is analyzed by the customer's operator.

To collect the crash dump, the docker container must be stopped, and then started again with the following parameters:

```
docker run -e CRASH_DUMP=true -v crash_dump_folder:/crash_dump ...
```

These parameters work with both the air and the cloak component. When a component is started in this way, the crash dump will be created in the `crash_dump_folder` on the host machine.

At this point, the system can be used normally. Once a crash happens, the operator needs to analyze the crash dump and send the relevant information to the Aircloak support. To analyze the crash dump, the operator can use the [Crashdump Viewer tool](http://erlang.org/doc/apps/observer/crashdump_ug.html). Note that Crashdump viewer requires an installation of Erlang (and possibly Elixir) on a desktop machine with a graphical windowing system. If this is not achievable, the operator can try to manually analyze the content of the crash dump file. More information on the crash dump content can be found [here](http://erlang.org/doc/apps/erts/crash_dump.html).

Once the Aircloak support team confirms that they have all the required information, the operator should stop the component, irrevocably remove the crash dump file, and start the component again, omitting the crashdump-specific parameters.
