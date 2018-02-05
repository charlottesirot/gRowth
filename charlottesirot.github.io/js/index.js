Vue.config.productionTip = false

Vue.component('start', {
  template: '#start'
})

Vue.component('ResearchInterest', {
  template: '#ResearchInterest'
})

Vue.component('publications', {
  template: '#publications'
})

Vue.component('softwares', {
  template: '#softwares'
})

Vue.component('CV', {
  template: '#CV'
})

new Vue({
  el: '#app',
  
  data: {
    page: 'start'
  },
  
  methods: {
    onChangePage(to) {
      this.page = to
    }
  }
})