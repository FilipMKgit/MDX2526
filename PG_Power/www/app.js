$(document).ready(function() {

  // -- Hints state (global, default ON) ------------------------------------
  window.pgpHintsOn = true;

  window.pgpToggleHints = function(btn) {
    window.pgpHintsOn = !window.pgpHintsOn;
    var lbl = document.getElementById('hints_toggle_label');
    if (lbl) lbl.textContent = window.pgpHintsOn ? 'Hide hints' : 'Show hints';
    btn.style.background  = window.pgpHintsOn ? '#f0eeff' : '';
    btn.style.borderColor = window.pgpHintsOn ? '#5b35d5' : '#e2e8f0';
    if (window.Shiny) {
      Shiny.setInputValue('show_calc_hints', window.pgpHintsOn, {priority: 'event'});
    }
  };

  // -- Code block toggle (overview) ----------------------------------------
  window.pgpToggleCode = function(id, hdr) {
    var body = document.getElementById(id);
    if (!body) return;
    var chev = hdr.querySelector('span:last-child');
    var isHidden = body.style.display === 'none' || body.style.display === '';
    body.style.display = isHidden ? 'block' : 'none';
    if (chev) chev.style.transform = isHidden ? 'rotate(180deg)' : '';
  };

  // -- n-box expand/collapse -----------------------------------------------
  window.pgpToggleNBox = function(btn) {
    var exp = document.getElementById('n_box_expanded');
    if (!exp) return;
    var isHidden = exp.style.display === 'none' || exp.style.display === '';
    exp.style.display = isHidden ? 'block' : 'none';
    btn.textContent   = isHidden ? 'collapse \u25b4' : 'expand \u25be';
  };

  // -- Plot colour picker ---------------------------------------------------
  window.pgpSetPlotColour = function(hex, btn) {
    document.querySelectorAll('.pgp-swatch').forEach(function(s) {
      s.classList.remove('active');
    });
    btn.classList.add('active');
    if (window.Shiny)
      Shiny.setInputValue('plot_colour', hex, {priority: 'event'});
  };

  // Initialise plot_colour on app load
  $(document).ready(function() {
    if (window.Shiny)
      Shiny.setInputValue('plot_colour', '#5b35d5', {priority: 'event'});
  });

  // -- Precision toggle ----------------------------------------------------
  window.pgpSetPrecision = function(dp, btn) {
    document.querySelectorAll('.pgp-prec-btn').forEach(function(b) {
      b.classList.remove('active');
    });
    if (btn) btn.classList.add('active');

    var is3dp = (dp === 3);

    if (is3dp) {
      if (window.Shiny) {
        var p0v = Shiny.shinyapp.$inputValues['p0.expected'] || 0.880;
        var p1v = Shiny.shinyapp.$inputValues['p1.expected'] || 0.930;
        var p0m = document.getElementById('p0.manual');
        var p1m = document.getElementById('p1.manual');
        if (p0m) p0m.value = parseFloat(p0v).toFixed(3);
        if (p1m) p1m.value = parseFloat(p1v).toFixed(3);
        Shiny.setInputValue('p0.manual', parseFloat(p0v), {priority: 'event'});
        Shiny.setInputValue('p1.manual', parseFloat(p1v), {priority: 'event'});
      }
    } else {
      if (window.Shiny) {
        var p0mv = Shiny.shinyapp.$inputValues['p0.manual'] || 0.880;
        var p1mv = Shiny.shinyapp.$inputValues['p1.manual'] || 0.930;
        var p0r  = Math.round(parseFloat(p0mv) * 100) / 100;
        var p1r  = Math.round(parseFloat(p1mv) * 100) / 100;
        Shiny.setInputValue('p0.expected', p0r, {priority: 'event'});
        Shiny.setInputValue('p1.expected', p1r, {priority: 'event'});
      }
    }

    if (window.Shiny) {
      Shiny.setInputValue('prop_precision', is3dp ? '3dp' : '2dp', {priority: 'event'});
    }
  };

  // Initialise precision
  $(document).ready(function() {
    if (window.Shiny) Shiny.setInputValue('prop_precision', '2dp', {priority: 'event'});
  });

  // -- Title templates -----------------------------------------------------
  window.titleTemplates = {
    'default':  'PG-Power \u2014 Sample Size Report',
    'study':    'Sample Size Calculation \u2014 Study Protocol',
    'clinical': 'Clinical Investigation: Sample Size Justification',
    'stats':    'Statistical Analysis Plan \u2014 Sample Size Section',
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
    'rpt_results', 'rpt_interp_inc', 'rpt_ci_compare', 'rpt_definitions',
    'rpt_calc_code', 'rpt_n_box', 'rpt_ci_diagram', 'rpt_plot_power',
    'rpt_plot_p1', 'rpt_table_p1'
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
      'rpt_definitions': true, 'rpt_calc_code': true, 'rpt_n_box': false,
      'rpt_ci_diagram': false, 'rpt_plot_power': false,
      'rpt_plot_p1': false, 'rpt_table_p1': false
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
    'blank': '',

    'default':
      'A total of {n} evaluable patients are required to demonstrate, with {power_pct}% ' +
      'power (one-sided \u03b1 = {alpha}), that the device success proportion meets the ' +
      'performance goal of {pg_pct}%, assuming a true success proportion of {pd_pct}%. ' +
      'Allowing for {dropout_pct}% dropout, the study will enrol {n_dropout} patients. ' +
      'The primary endpoint will be met if {decision_rule}.',

    'concise':
      'A sample size of {n} evaluable patients (enrolment target: {n_dropout} with ' +
      '{dropout_pct}% dropout) provides {power_pct}% power at a one-sided significance ' +
      'level of {alpha} to demonstrate that the device success proportion exceeds the ' +
      'performance goal of {pg_pct}%, assuming a true proportion of {pd_pct}%. ' +
      'Success is declared if {decision_rule}.',

    'success_ci':
      'This study uses a single-arm design with a pre-specified performance goal (PG) of ' +
      '{pg_pct}%. A sample size of {n} evaluable patients is required such that, assuming ' +
      'a true success proportion of {pd_pct}%, the {ci_method} one-sided {alpha}-level ' +
      'confidence interval lower bound for the observed proportion exceeds the PG with ' +
      '{power_pct}% probability. Accounting for {dropout_pct}% anticipated dropout, ' +
      '{n_dropout} patients will be enrolled. The study meets its primary endpoint if ' +
      '{decision_rule}.',

    'success_power':
      'The sample size of {n} evaluable patients was determined using a one-sided ' +
      'binomial confidence interval approach. With an assumed true success proportion of ' +
      '{pd_pct}% and a performance goal of {pg_pct}%, this sample size achieves ' +
      '{power_pct}% power at a one-sided significance level of {alpha} using the ' +
      '{ci_method} method. An enrolment target of {n_dropout} patients accounts for ' +
      '{dropout_pct}% expected dropout. The primary success criterion is {decision_rule}.',

    'regulatory':
      'The study is designed as a single-arm, prospective investigation with a ' +
      'pre-specified objective performance criterion (OPC) of {pg_pct}%, derived from ' +
      'published literature and historical device data. A minimum of {n} evaluable ' +
      'subjects is required to demonstrate, with {power_pct}% power at a one-sided ' +
      'significance level of \u03b1 = {alpha}, that the lower bound of the {ci_method} ' +
      'confidence interval for the primary endpoint proportion exceeds the OPC. ' +
      'Accounting for {dropout_pct}% dropout, {n_dropout} subjects will be enrolled. ' +
      'In accordance with ISO 14155 requirements, the study will be deemed successful ' +
      'if {decision_rule}.',

    'safety':
      'A total of {n} evaluable patients are required to demonstrate, with {power_pct}% ' +
      'power (one-sided \u03b1 = {alpha}), that the device complication proportion does ' +
      'not exceed the performance goal of {pg_pct}%, assuming a true complication ' +
      'proportion of {pd_pct}%. Allowing for {dropout_pct}% dropout, {n_dropout} ' +
      'patients will be enrolled. The safety endpoint will be satisfied if {decision_rule}.',

    'safety_ci':
      'This single-arm study is powered to demonstrate that the upper bound of the ' +
      '{ci_method} confidence interval (one-sided \u03b1 = {alpha}) for the observed ' +
      'complication proportion falls below the pre-specified performance goal of {pg_pct}%, ' +
      'assuming a true complication proportion of {pd_pct}%. A sample size of {n} ' +
      'evaluable patients ({n_dropout} enrolled, allowing for {dropout_pct}% dropout) ' +
      'provides {power_pct}% power to meet this criterion. ' +
      'The endpoint is met if {decision_rule}.',

    'safety_reg':
      'The study employs a single-arm design to evaluate device safety against a ' +
      'pre-specified objective performance criterion (OPC) of {pg_pct}% for the primary ' +
      'safety endpoint. Consistent with ISO 14155:2020 and applicable guidance, a minimum ' +
      'of {n} evaluable subjects is required to provide {power_pct}% power at a one-sided ' +
      'significance level of \u03b1 = {alpha}, using the {ci_method} method to construct ' +
      'the confidence interval upper bound. With {dropout_pct}% anticipated dropout, the ' +
      'enrolment target is {n_dropout} subjects. The primary safety criterion is satisfied ' +
      'if {decision_rule}.'
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

  // -- Full app reset -------------------------------------------------------
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

    window.pgpHintsOn = true;
    var lbl = document.getElementById('hints_toggle_label');
    if (lbl) lbl.textContent = 'Hide hints';
    var btn = document.getElementById('hints_toggle_btn');
    if (btn) { btn.style.background = '#f0eeff'; btn.style.borderColor = '#5b35d5'; }
    S.setInputValue('show_calc_hints', true, {priority: 'event'});

    // Reset plot colour to purple
    document.querySelectorAll('.pgp-swatch').forEach(function(s) {
      s.classList.remove('active');
    });
    var ps = document.getElementById('swatch_purple');
    if (ps) ps.classList.add('active');
    S.setInputValue('plot_colour', '#5b35d5', {priority: 'event'});
  };

  window.pgpResetCalculator = function() {
    var S = window.Shiny;
    if (!S) return;
    S.setInputValue('endpoint',         'efficacy', {priority: 'event'});
    S.setInputValue('sig.level',        '0.025',    {priority: 'event'});
    S.setInputValue('power',             0.90,      {priority: 'event'});
    S.setInputValue('ci_method_prop',   'exact',    {priority: 'event'});
    window.pgpSetPrecision(2, document.getElementById('prec_2dp'));
    S.setInputValue('p0.expected',       0.880,     {priority: 'event'});
    S.setInputValue('p0.manual',         0.880,     {priority: 'event'});
    S.setInputValue('p1.expected',       0.930,     {priority: 'event'});
    S.setInputValue('p1.manual',         0.930,     {priority: 'event'});
    S.setInputValue('sim_quality',      '1000',     {priority: 'event'});
    S.setInputValue('sim_seed',          1,         {priority: 'event'});
    S.setInputValue('show_calc_code',    false,     {priority: 'event'});
    S.setInputValue('showNBox_prop',     true,      {priority: 'event'});
    S.setInputValue('showVline',         false,     {priority: 'event'});
    S.setInputValue('showTable2',        false,     {priority: 'event'});
    S.setInputValue('showCIDiagram',     false,     {priority: 'event'});
    S.setInputValue('showAllCI',         false,     {priority: 'event'});
    S.setInputValue('showPowerTable',    false,     {priority: 'event'});
    S.setInputValue('dropout_rate',      10,        {priority: 'event'});
    S.setInputValue('power_plot_range',  50,        {priority: 'event'});
  };

  // -- GitHub popup --------------------------------------------------------
  window.pgpClosePopup = function() {
    var p = document.getElementById('pgp-gh-popup');
    if (p) p.remove();
  };

  Shiny.addCustomMessageHandler('showGithubPopup', function(url) {
    window.pgpClosePopup();
    var d = document.createElement('div');
    d.id = 'pgp-gh-popup';
    d.style.cssText =
      'position:fixed;top:50%;left:50%;transform:translate(-50%,-50%);' +
      'background:#fff;border:1px solid #e2e8f0;border-radius:12px;' +
      'padding:24px 28px;z-index:9999;box-shadow:0 8px 32px rgba(0,0,0,0.18);' +
      'max-width:380px;width:90%;font-family:DM Sans,sans-serif;';
    d.innerHTML =
      '<div style="display:flex;align-items:center;justify-content:space-between;margin-bottom:12px;">' +
      '  <span style="font-weight:700;font-size:14px;color:#1a2e35;">Source code</span>' +
      '  <button onclick="window.pgpClosePopup();" ' +
      '    style="background:none;border:none;cursor:pointer;font-size:18px;color:#94a3b8;">&#x2715;</button>' +
      '</div>' +
      '<p style="font-size:12px;color:#374151;margin:0 0 14px;line-height:1.6;">' +
      'The full source code for PG-Power is available on GitHub.</p>' +
      '<a href="' + url + '" target="_blank" style="display:inline-flex;align-items:center;' +
      'gap:6px;background:#5b35d5;color:#fff;text-decoration:none;padding:8px 16px;' +
      'border-radius:7px;font-size:12px;font-weight:600;">&#x2197; Open on GitHub</a>' +
      '<button onclick="window.pgpClosePopup();" ' +
      'style="display:inline-block;margin-left:10px;background:none;border:1px solid #e2e8f0;' +
      'border-radius:7px;padding:8px 14px;font-size:12px;color:#374151;cursor:pointer;">Close</button>';
    document.body.appendChild(d);
    setTimeout(function() {
      document.addEventListener('click', function pgpClose(e) {
        if (!d.contains(e.target)) {
          d.remove();
          document.removeEventListener('click', pgpClose);
        }
      });
    }, 100);
  });

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
