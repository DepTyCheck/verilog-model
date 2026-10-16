-- Seed: 17038194988287780395,9900838946149614537

entity yjndr is
  port (pfum : inout boolean; acrczpqydv : inout real_vector(3 downto 2));
end yjndr;

architecture f of yjndr is
  
begin
  -- Single-driven assignments
  acrczpqydv <= acrczpqydv;
  pfum <= TRUE;
end f;

entity ulcnghsb is
  port (oibmp : buffer integer; ae : linkage real; fxwlyj : inout time; poknkpe : inout integer);
end ulcnghsb;

architecture eouh of ulcnghsb is
  signal uhtahc : real_vector(3 downto 2);
  signal mbkqr : boolean;
  signal ga : real_vector(3 downto 2);
  signal wrhglpeh : boolean;
  signal ycsd : real_vector(3 downto 2);
  signal tllpj : boolean;
begin
  gexhbknld : entity work.yjndr
    port map (pfum => tllpj, acrczpqydv => ycsd);
  xv : entity work.yjndr
    port map (pfum => wrhglpeh, acrczpqydv => ga);
  czyoaosqz : entity work.yjndr
    port map (pfum => mbkqr, acrczpqydv => uhtahc);
  
  -- Single-driven assignments
  poknkpe <= 8#77775#;
  oibmp <= poknkpe;
  fxwlyj <= 16#647# ns;
end eouh;

library ieee;
use ieee.std_logic_1164.all;

entity qbofqbc is
  port (afspb : buffer integer; sug : out std_logic_vector(1 to 0));
end qbofqbc;

architecture nimllxu of qbofqbc is
  signal qoibw : time;
  signal cxxaepl : real;
  signal pph : integer;
  signal drwqwt : real_vector(3 downto 2);
  signal as : boolean;
  signal wzlku : real_vector(3 downto 2);
  signal nwe : boolean;
begin
  twtpkfc : entity work.yjndr
    port map (pfum => nwe, acrczpqydv => wzlku);
  bdeqyoj : entity work.yjndr
    port map (pfum => as, acrczpqydv => drwqwt);
  o : entity work.ulcnghsb
    port map (oibmp => pph, ae => cxxaepl, fxwlyj => qoibw, poknkpe => afspb);
end nimllxu;



-- Seed after: 12967570663921347786,9900838946149614537
