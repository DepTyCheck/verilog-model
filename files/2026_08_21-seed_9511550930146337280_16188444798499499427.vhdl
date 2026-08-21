-- Seed: 9511550930146337280,16188444798499499427

entity dizcxdt is
  port (efag : in character; x : inout real; s : inout bit);
end dizcxdt;

architecture pu of dizcxdt is
  
begin
  -- Single-driven assignments
  s <= s;
  x <= 2#1_1_0_1.00110#;
end pu;

entity dwtzobkoh is
  port (hnsf : in severity_level; yjhkndn : in integer; glq : out real);
end dwtzobkoh;

architecture xawnp of dwtzobkoh is
  signal srjwftnxo : bit;
  signal rxmzhsy : real;
  signal jbthlptv : bit;
  signal rxtlfb : character;
  signal cjiimqr : bit;
  signal oobvor : real;
  signal rbyk : character;
begin
  awma : entity work.dizcxdt
    port map (efag => rbyk, x => oobvor, s => cjiimqr);
  ntlkti : entity work.dizcxdt
    port map (efag => rxtlfb, x => glq, s => jbthlptv);
  cdjybhfmtl : entity work.dizcxdt
    port map (efag => rbyk, x => rxmzhsy, s => srjwftnxo);
  
  -- Single-driven assignments
  rbyk <= rbyk;
end xawnp;

entity ybfpjwgj is
  port (sy : out real);
end ybfpjwgj;

architecture ralo of ybfpjwgj is
  signal pdijmdbmvb : bit;
  signal dtawpuvl : character;
  signal pkhrwr : bit;
  signal tuvnamtm : real;
  signal bcyusiw : character;
  signal meifijd : real;
  signal nfi : integer;
  signal sr : severity_level;
begin
  ebqdn : entity work.dwtzobkoh
    port map (hnsf => sr, yjhkndn => nfi, glq => meifijd);
  btrr : entity work.dizcxdt
    port map (efag => bcyusiw, x => tuvnamtm, s => pkhrwr);
  dkg : entity work.dizcxdt
    port map (efag => dtawpuvl, x => sy, s => pdijmdbmvb);
  
  -- Single-driven assignments
  nfi <= nfi;
  bcyusiw <= bcyusiw;
  sr <= WARNING;
  dtawpuvl <= 'c';
end ralo;



-- Seed after: 7328696480492686100,16188444798499499427
