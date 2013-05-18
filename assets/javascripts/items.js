(function() {
  var loading = false;

  function markRead() {
    var $item = $(this);
    if ($item.data('markedRead') == null) {
      $item.data('markedRead', true);
      $.ajax({
        url: '/read',
        data: {id: $item.attr('data-id')},
        type: 'POST'
      }).success(function(data) {
        $item.addClass('is_read');
      }).error(function() {
      }).complete(function() {
        loading = false;
      });
    }
  }

  function loadMore() {
    if (!loading) {
      loading = true;
      var params = {};
      var maxId = $("li[data-id]").last().attr('data-id');
      if (maxId) {
        params['beforeId'] = maxId;
      }
      $.ajax({
        url: '/items',
        data: params,
        type: 'GET'
      }).success(function(data) {
        var $items = $(data);
        $items.each(function() {
          var $item = $(this);
          $item.on('mouseover', markRead);
        });
        $("#items").append($items);
      }).error(function() {
      }).complete(function() {
        loading = false;
      });
    }
  }

  $(document).on('scroll', function() {
    if ($(document).scrollTop() + $(window).innerHeight() >= $(document).innerHeight()) {
      loadMore();
    }
  });

  $(loadMore);
})();