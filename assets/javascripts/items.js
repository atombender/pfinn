(function() {
  var loading = false;
  var offset = 0;

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
      $.ajax({
        url: '/items',
        data: {offset: offset},
        type: 'GET'
      }).success(function(data) {
        var $items = $(data);
        $items.each(function() {
          offset++;
          var $item = $(this);
          var id = parseInt($item.attr('data-id'));
          $item.on('mouseover', function() {
            markRead(id);
          });
          $item.find('.actions')
            .append(
              $('<a>')
                .attr('href', '#')
                .on('click', function(event) {
                  event.preventDefault();
                  $.ajax({
                    url: '/ignore',
                    data: {id: id},
                    type: 'POST'
                  }).success(function(data) {
                    $item.addClass('is_ignored');
                  }).error(function() {
                  }).complete(function() {
                  });
                })
                .text('Ignorer'));
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