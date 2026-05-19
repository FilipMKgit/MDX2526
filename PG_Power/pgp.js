$(document).ready(function() {

    // -- Hints state (global, default ON) ------------------------------------
    window.pgpHintsOn = true;

    window.pgpToggleHints = function(btn) {
      window.pgpHintsOn = !window.pgpHintsOn;
      var lbl = document.getElementById('hints_toggle_label');
      if (lbl) lbl.textContent = window.pgpHintsOn ? 'Show hints' : 'Hide hints';
      btn.style.background   = window.pgpHintsOn ? '' : '#eef9f9';
      btn.style.borderColor  = window.pgpHintsOn ? '#e2e8f0' : '#18bdb9';
      if (window.Shiny) {
        Shiny.setInputValue('show_calc_hints',    window.pgpHintsOn, {priority: 'event'});
        Shiny.setInputValue('show_interim_hints', window.pgpHintsOn, {priority: 'event'});
      }
    };

    // -- n-box expand/collapse -----------------------------------------------
    window.pgpToggleNBox = function(btn) {
      var exp = document.getElementById('n_box_expanded');
      if (!exp) return;
      var isHidden = exp.style.display === 'none' || exp.style.display === '';
      exp.style.display = isHidden ? 'block' : 'none';
      btn.textContent   = isHidden ? 'collapse ▴' : 'expand ▾';
    };

    // -- Title templates -----------------------------------------------------
    window.titleTemplates = {
      'default':  'PG-Power — Sample Size Report',
      'study':    'Sample Size Calculation — Study Protocol',
      'clinical': 'Clinical Investigation: Sample Size Justification',
      'stats':    'Statistical Analysis Plan — Sample Size Section',
      'blank':    ''
    };

    window.pgpSetTitle = function(txt) {
      var el = document.getElementById('rpt_title');
      if (!el) return;
      el.value = txt;
      if (window.Shiny) Shiny.setInputValue('rpt_title', txt, {priority: 'event'});
      el.dispatchEvent(new Event('input', {bubbles: true}));
    };

    window.pgpLoadTitleTemplate = function() {
      var sel = document.getElementById('title_template_select');
      var key = sel ? sel.value : 'default';
      var txt = window.titleTemplates[key];
      if (txt === undefined) txt = window.titleTemplates['default'];
      window.pgpSetTitle(txt);
    };

    window.pgpRestoreTitleDefault = function() {
      window.pgpSetTitle(window.titleTemplates['default']);
      var sel = document.getElementById('title_template_select');
      if (sel) sel.value = 'default';
    };

    // -- Include checkboxes --------------------------------------------------
    var pgpIncludeIds = [
        'rpt_results','rpt_interp_inc','rpt_ci_compare','rpt_definitions',
        'rpt_calc_code','rpt_plot_delta','rpt_plot_p1','rpt_table_delta',
        'rpt_table_p1','rpt_interim_summ','rpt_interim_interp',
        'rpt_interim_ci','rpt_interim_plot'
      ];

    window.pgpTickAllIncludes = function() {
      pgpIncludeIds.forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = true;
        if (window.Shiny) Shiny.setInputValue(id, true, {priority: 'event'});
      });
    };

    window.pgpUntickAllIncludes = function() {
      pgpIncludeIds.forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = false;
        if (window.Shiny) Shiny.setInputValue(id, false, {priority: 'event'});
      });
    };

    window.pgpRestoreIncludes = function() {
      var defaults = {
        'rpt_results': true, 'rpt_interp_inc': true, 'rpt_ci_compare': false,
        'rpt_definitions': true, 'rpt_calc_code': true,
        'rpt_plot_delta': false, 'rpt_plot_p1': false,
        'rpt_table_delta': false, 'rpt_table_p1': false,
        'rpt_interim_summ': false, 'rpt_interim_interp': false,
        'rpt_interim_ci': false, 'rpt_interim_plot': false
      };
      Object.keys(defaults).forEach(function(id) {
        var cb = document.getElementById(id);
        if (!cb) return;
        cb.checked = defaults[id];
        if (window.Shiny) Shiny.setInputValue(id, defaults[id], {priority: 'event'});
      });
    };

    // -- Interpretation templates --------------------------------------------
    window.interpTemplates = {
      'blank':      '',
      'default':    'A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power, that the device success rate exceeds the performance goal of {p0_pct}%, assuming a true success rate of {p1_pct}%. Allowing for {dropout_pct}% dropout, the study should enrol {n_dropout} patients. The study will be deemed successful if at least {n_successes} out of {n} evaluable patients are free from a major adverse event at 12 months.',
      'concise':    'A sample size of {n} patients provides {power_pct}% power (one-sided α = {alpha}) to demonstrate non-inferiority of the device against the performance goal of {p0_pct}%, with an NI margin of Δ = {delta}, assuming a true device success rate of {p1_pct}%.',
      'two_arm':    'A total of {n} patients are required to demonstrate non-inferiority of the treatment versus the control, with {power_pct}% power and a one-sided significance level of {alpha}. The assumed event rates are {p1_pct}% (treatment) and {p0_pct}% (control), with a non-inferiority margin of {delta} on the risk difference scale. Allowing for {dropout_pct}% dropout, {n_dropout} patients should be enrolled.',
      'regulatory': 'The study is designed as a single-arm, non-inferiority study comparing the device success rate to an objective performance criterion (OPC) of {p0_pct}%, consistent with published literature and historical data. A minimum of {n} evaluable subjects is required to demonstrate, with {power_pct}% power at a one-sided significance level of {alpha}, that the lower bound of the {ci_method} confidence interval for the device success rate exceeds the performance goal less the non-inferiority margin ({delta}). Accounting for a {dropout_pct}% dropout rate, the study will enrol {n_dropout} subjects. The primary endpoint will be met if at least {n_successes} of {n} evaluable subjects achieve procedural success.',
      'safety':     'A total of {n} evaluable patients are required to demonstrate, with {power_pct}% power (one-sided α = {alpha}), that the device complication rate is non-inferior to the performance goal of {p0_pct}%, assuming a true complication rate of {p1_pct}% and an acceptable margin of {delta}. With an anticipated dropout rate of {dropout_pct}%, {n_dropout} patients will be enrolled. The safety endpoint will be satisfied if no more than the pre-specified number of adverse events are observed among the {n} evaluable patients.'
    };

    window.pgpSetInterp = function(txt) {
      var ta = document.getElementById('rpt_interp_text');
      if (!ta) return;
      ta.value = txt;
      if (window.Shiny) Shiny.setInputValue('rpt_interp_text', txt, {priority: 'event'});
    };

    window.pgpLoadTemplate = function() {
      var sel = document.getElementById('interp_template_select');
      var key = sel ? sel.value : 'default';
      var txt = (window.interpTemplates[key] !== undefined)
                  ? window.interpTemplates[key]
                  : window.interpTemplates['default'];
      window.pgpSetInterp(txt);
    };

    window.pgpRestoreDefault = function() {
      window.pgpSetInterp(window.interpTemplates['default']);
      var sel = document.getElementById('interp_template_select');
      if (sel) sel.value = 'default';
    };

    // -- Full app reset ------------------------------------------------------
    window.pgpResetAll = function() {
      window.pgpResetCalculator();
      window.pgpRestoreTitleDefault();
      window.pgpRestoreDefault();
      window.pgpRestoreIncludes();

      var S = window.Shiny;
      if (!S) return;
      S.setInputValue('rpt_include_date',   true,  {priority: 'event'});
      S.setInputValue('rpt_include_method', true,  {priority: 'event'});
      S.setInputValue('rpt_include_author', false, {priority: 'event'});
      S.setInputValue('report_format',      'pdf', {priority: 'event'});

      S.setInputValue('interim_n',         0, {priority: 'event'});
      S.setInputValue('interim_x',         0, {priority: 'event'});
      S.setInputValue('interim_x_control', 0, {priority: 'event'});

      window.pgpResetInterimSettings();

      window.pgpHintsOn = true;
      var lbl = document.getElementById('hints_toggle_label');
      if (lbl) lbl.textContent = 'Show hints';
      var btn = document.getElementById('hints_toggle_btn');
      if (btn) { btn.style.background = ''; btn.style.borderColor = '#e2e8f0'; }
      S.setInputValue('show_calc_hints',    true, {priority: 'event'});
      S.setInputValue('show_interim_hints', true, {priority: 'event'});
    };

    window.pgpResetInterimSettings = function() {
      var S = window.Shiny;
      if (!S) return;
      S.setInputValue('show_interim_calctbl', true,  {priority: 'event'});
      S.setInputValue('show_interim_citbl',   true,  {priority: 'event'});
      S.setInputValue('show_interim_code',    false, {priority: 'event'});
    };

    window.pgpResetCalculator = function() {
      var S = window.Shiny;
      if (!S) return;
      S.setInputValue('prop_design',    'one_arm',  {priority: 'event'});
      S.setInputValue('endpoint',       'efficacy', {priority: 'event'});
      S.setInputValue('sig.level',      0.025,      {priority: 'event'});
      S.setInputValue('power',          0.80,       {priority: 'event'});
      S.setInputValue('r',              '1',        {priority: 'event'});
      S.setInputValue('ci_method_prop', 'wilson',   {priority: 'event'});
      S.setInputValue('showCompare',    false,      {priority: 'event'});
      S.setInputValue('p0.expected',    0.88,       {priority: 'event'});
      S.setInputValue('p1.expected',    0.93,       {priority: 'event'});
      S.setInputValue('p1.tolerable',   0.05,       {priority: 'event'});
      S.setInputValue('WindowMargin',   '0.05',     {priority: 'event'});
      S.setInputValue('sim_quality',    '1000',     {priority: 'event'});
      S.setInputValue('sim_seed',       1,          {priority: 'event'});
      S.setInputValue('show_calc_code', false,      {priority: 'event'});
      S.setInputValue('showNBox_prop',  true,       {priority: 'event'});
      S.setInputValue('showVline',      false,      {priority: 'event'});
      S.setInputValue('showTable',      false,      {priority: 'event'});
      S.setInputValue('showTable2',     false,      {priority: 'event'});
      S.setInputValue('dropout_rate',   10,         {priority: 'event'});
    };

    // -- Accordion toggle ----------------------------------------------------
    $(document).on('click', '.pgp-accordion-header', function() {
      var $hdr  = $(this);
      var $body = $hdr.next('.pgp-accordion-body');
      $hdr.toggleClass('open');
      $body.toggleClass('open');
    });

    // -- Sync interp textarea to Shiny ---------------------------------------
    var ta = document.getElementById('rpt_interp_text');
    if (ta) {
      Shiny.setInputValue('rpt_interp_text', ta.value);
      ta.addEventListener('input', function() {
        Shiny.setInputValue('rpt_interp_text', ta.value, {priority: 'event'});
      });
    }

});
