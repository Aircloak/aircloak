defmodule Air.TaskPostProcessorTest do
  use ExUnit.Case, async: true

  alias Air.TaskPostProcessor

  test "post-processing without changes" do
    original_result = %{
      "task_id" => "1",
      "buckets" => [%{"label" => "data", "value" => "1", "count" => 10}]
    }
    result = TaskPostProcessor.process(original_result)
    assert "1" == result["task_id"]
    assert [%{"count" => 10, "label" => "data", "value" => "1"}] = result["buckets"]
    assert nil == result["post_processed"]["histograms"]
    assert %{} == result["post_processed"]["aircloak"]
  end

  test "post-processing extract aircloak buckets" do
    original_result = %{
      "task_id" => "1",
      "buckets" => [%{"label" => "aircloak", "value" => "lcf_tail", "count" => 10}]
    }
    result = TaskPostProcessor.process(original_result)
    assert [] = result["buckets"]
    assert %{"lcf_tail" => 10} = result["post_processed"]["aircloak"]
  end

  test "post-processing a histogram" do
    original_result = %{
      "task_id" => "1",
      "buckets" => [
        %{"label" => "data", "value" => "<10", "count" => 30},
        %{"label" => "data", "value" => "[-100,100,10]", "count" => 80},
        %{"label" => "data", "value" => "<80", "count" => 80},
        %{"label" => "quantized", "value" => "data", "count" => 80},
        %{"label" => "data", "value" => "<30", "count" => 60},
        %{"label" => "data", "value" => "<20", "count" => 45},
        %{"label" => "id", "value" => "12", "count" => 5},
        %{"label" => "data", "value" => "<60", "count" => 80},
        %{"label" => "data", "value" => "<70", "count" => 80},
        %{"label" => "data", "value" => "<-10", "count" => 25},
        %{"label" => "id", "value" => "1", "count" => 10},
        %{"label" => "data", "value" => "<-20", "count" => 15},
        %{"label" => "id", "value" => "13", "count" => 10},
        %{"label" => "id", "value" => "7", "count" => 10},
        %{"label" => "id", "value" => "10", "count" => 5},
        %{"label" => "data", "value" => "<40", "count" => 75},
        %{"label" => "data", "value" => "<0", "count" => 30},
        %{"label" => "data", "value" => "<50", "count" => 85},
        %{"label" => "data", "value" => "<90", "count" => 80},
        %{"label" => "id", "value" => "14", "count" => 5}
      ],
      "exceptions" => [],
    }
    result = TaskPostProcessor.process(original_result)
    histograms = result["post_processed"]["histograms"]
    assert [%{"max" => 100.0, "min" => -100.0, "name" => "data", "values" => [_|_]}] = histograms
  end
end
