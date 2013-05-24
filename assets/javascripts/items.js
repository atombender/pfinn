(function() {
  var loading = false;

  function markRead(id) {
    var top = $("li[data-id=" + id + "]").offset().top;
    $("li[data-id]").each(function() {
      var $item = $(this);
      if ($item.offset().top <= top) {
        var anId = parseInt($item.attr('data-id'));
        if ($item.data('markedRead') == null) {
          $item.data('markedRead', true);
          $.ajax({
            url: '/read',
            data: {id: anId},
            type: 'POST'
          }).success(function(data) {
            $item.addClass('is_read');
          }).error(function() {
          }).complete(function() {
            loading = false;
          });
        }
      }
    });
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
          $item.on('mouseover', function() {
            var $item = $(this);
            var id = parseInt($item.attr('data-id'));
            markRead(id);
          });
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