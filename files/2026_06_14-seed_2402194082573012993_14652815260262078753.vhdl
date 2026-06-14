-- Seed: 2402194082573012993,14652815260262078753

entity kgorjnhko is
  port (bxwyce : out boolean_vector(4 to 4); qbwr : inout time; fm : linkage severity_level);
end kgorjnhko;

architecture zncaaa of kgorjnhko is
  
begin
  
end zncaaa;

entity qo is
  port (gsxoujxu : out real);
end qo;

architecture ua of qo is
  signal ufhrjdhvb : severity_level;
  signal lrmtge : time;
  signal xljcxwzw : boolean_vector(4 to 4);
  signal mbcoxb : severity_level;
  signal faksutqvge : time;
  signal fzfbjkmbrh : boolean_vector(4 to 4);
  signal pcuigazh : severity_level;
  signal ik : time;
  signal ebfmol : boolean_vector(4 to 4);
  signal ws : severity_level;
  signal wouybykrc : time;
  signal ehbm : boolean_vector(4 to 4);
begin
  p : entity work.kgorjnhko
    port map (bxwyce => ehbm, qbwr => wouybykrc, fm => ws);
  qqnzmazex : entity work.kgorjnhko
    port map (bxwyce => ebfmol, qbwr => ik, fm => pcuigazh);
  rophoe : entity work.kgorjnhko
    port map (bxwyce => fzfbjkmbrh, qbwr => faksutqvge, fm => mbcoxb);
  ixuiv : entity work.kgorjnhko
    port map (bxwyce => xljcxwzw, qbwr => lrmtge, fm => ufhrjdhvb);
  
  -- Single-driven assignments
  gsxoujxu <= 8#376.1_5_7#;
end ua;

entity bkekn is
  port (svyiq : in real; rck : out severity_level; r : buffer real);
end bkekn;

architecture firjueegk of bkekn is
  signal beg : real;
  signal hdavj : real;
  signal yezt : time;
  signal uxuosiz : boolean_vector(4 to 4);
begin
  zv : entity work.kgorjnhko
    port map (bxwyce => uxuosiz, qbwr => yezt, fm => rck);
  empqvvqx : entity work.qo
    port map (gsxoujxu => hdavj);
  mdeshts : entity work.qo
    port map (gsxoujxu => beg);
end firjueegk;



-- Seed after: 4261177536850395838,14652815260262078753
