library('testthat')

test_that('cache invalidation works', {
  scenarios <- tibble(start_with=33, add_once=11, add_once_more=22)
  cache <- make_cache(scenarios)

  scenario <- scenarios[1,]
  step <- step_through(scenario, cache)

  expect_mapequal(as.list(cache), list(start_with=NA, add_once=NA, add_once_more=NA))

  step('start_with', function(input) {
    list(data=scenario$start_with)
  })

  expect_mapequal(as.list(cache), list(start_with=list(data=33), add_once=NA, add_once_more=NA))

  step('add_once', function(input) {
    list(data=input$data + 11)
  })

  expect_mapequal(as.list(cache), list(start_with=list(data=33), add_once=list(data=44), add_once_more=NA))

  step('add_once_more', function(input) {
    list(data=input$data + 22)
  })

  expect_mapequal(as.list(cache), list(start_with=list(data=33), add_once=list(data=44), add_once_more=list(data=66)))

  # should be a no-op because cache hasn't been cleared yet!
  step('add_once', function(input) {
    list(data=input$data + 0)
  })

  expect_equal(cache$add_once$data, 44)

  # but after invalidation, it should do the process
  invalidate_cache(cache, 2)
  # as.list(cache)

  step('add_once', function(input) {
    list(data=input$data + 0)
  })

  expect_equal(cache$add_once$data, 33)

  # as.list(cache)
})
