-- Seed: 13938766041605162915,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity zgsnrr is
  port (dt : linkage std_logic; ofxuzo : inout std_logic; pqf : out std_logic_vector(3 downto 3); qe : in std_logic_vector(4 downto 0));
end zgsnrr;

architecture lqjt of zgsnrr is
  
begin
  -- Multi-driven assignments
  pqf <= (others => 'H');
end lqjt;

library ieee;
use ieee.std_logic_1164.all;

entity ucnsgmczz is
  port (zljkqcil : buffer integer; cxblwzwro : in std_logic_vector(2 to 3); szo : inout integer; mitalkwkqd : in real);
end ucnsgmczz;

library ieee;
use ieee.std_logic_1164.all;

architecture mzdglzhjez of ucnsgmczz is
  signal pttgp : std_logic;
  signal aqcugig : std_logic_vector(3 downto 3);
  signal myy : std_logic_vector(4 downto 0);
  signal mzyb : std_logic;
  signal xygkodm : std_logic_vector(4 downto 0);
  signal nxrrx : std_logic_vector(3 downto 3);
  signal nzlerlbwm : std_logic;
  signal cwitghv : std_logic;
begin
  ifwvklnz : entity work.zgsnrr
    port map (dt => cwitghv, ofxuzo => nzlerlbwm, pqf => nxrrx, qe => xygkodm);
  izwngygubn : entity work.zgsnrr
    port map (dt => mzyb, ofxuzo => cwitghv, pqf => nxrrx, qe => myy);
  ggbw : entity work.zgsnrr
    port map (dt => cwitghv, ofxuzo => cwitghv, pqf => aqcugig, qe => xygkodm);
  ozgzpdnhp : entity work.zgsnrr
    port map (dt => cwitghv, ofxuzo => pttgp, pqf => nxrrx, qe => xygkodm);
  
  -- Single-driven assignments
  szo <= 0_1_2_4_3;
  zljkqcil <= 16#F09#;
  
  -- Multi-driven assignments
  cwitghv <= 'Z';
  mzyb <= '-';
end mzdglzhjez;

entity n is
  port (eweumrs : buffer character; zkkvets : linkage real; pcqbx : linkage integer; jzs : out integer_vector(0 downto 1));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture islagyjmp of n is
  signal fqmmu : std_logic_vector(4 downto 0);
  signal nux : std_logic_vector(4 downto 0);
  signal v : std_logic_vector(3 downto 3);
  signal oooxny : std_logic;
  signal ugurx : std_logic;
begin
  rfmqa : entity work.zgsnrr
    port map (dt => ugurx, ofxuzo => oooxny, pqf => v, qe => nux);
  fob : entity work.zgsnrr
    port map (dt => ugurx, ofxuzo => oooxny, pqf => v, qe => fqmmu);
  
  -- Single-driven assignments
  eweumrs <= 'a';
  jzs <= (others => 0);
  
  -- Multi-driven assignments
  fqmmu <= "1X0ZX";
  ugurx <= 'W';
  nux <= "UH0HH";
  nux <= ('-', 'Z', '-', '1', '1');
end islagyjmp;

entity yuyevuopuu is
  port (hasoqg : in integer; psh : out real);
end yuyevuopuu;

architecture pml of yuyevuopuu is
  
begin
  
end pml;



-- Seed after: 14359431753438407047,15300320181035395489
