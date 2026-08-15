-- Seed: 12443299209385810497,2230106469645304029

entity eppypf is
  port (zpm : buffer severity_level; uoccepq : linkage integer; siry : out integer; jgcrmmvzmu : buffer real);
end eppypf;

architecture vfeahu of eppypf is
  
begin
  -- Single-driven assignments
  zpm <= zpm;
  siry <= 2#0#;
  jgcrmmvzmu <= 031.3314;
end vfeahu;

entity mqwf is
  port (t : linkage real);
end mqwf;

architecture dbnolzvku of mqwf is
  signal venmocif : real;
  signal syrphndbqx : integer;
  signal pe : integer;
  signal rhngnoad : severity_level;
  signal lb : real;
  signal tclszfcyee : integer;
  signal hceep : integer;
  signal balxxliena : severity_level;
  signal sfgfgz : real;
  signal qfcgvy : integer;
  signal guejhwtc : integer;
  signal ittyxclft : severity_level;
  signal lbbikjxe : real;
  signal qixizuy : integer;
  signal cvz : integer;
  signal xhk : severity_level;
begin
  apr : entity work.eppypf
    port map (zpm => xhk, uoccepq => cvz, siry => qixizuy, jgcrmmvzmu => lbbikjxe);
  w : entity work.eppypf
    port map (zpm => ittyxclft, uoccepq => guejhwtc, siry => qfcgvy, jgcrmmvzmu => sfgfgz);
  igbkpmd : entity work.eppypf
    port map (zpm => balxxliena, uoccepq => hceep, siry => tclszfcyee, jgcrmmvzmu => lb);
  hqd : entity work.eppypf
    port map (zpm => rhngnoad, uoccepq => pe, siry => syrphndbqx, jgcrmmvzmu => venmocif);
end dbnolzvku;

entity sbxhf is
  port (a : linkage time; zdnasrzenx : linkage time_vector(2 downto 2); ilyzaywb : inout real);
end sbxhf;

architecture zkxbcrxhr of sbxhf is
  
begin
  
end zkxbcrxhr;

library ieee;
use ieee.std_logic_1164.all;

entity bmzzcr is
  port (gggufpas : buffer real; idhjuu : out integer; qwx : inout std_logic; ikg : in bit);
end bmzzcr;

architecture jc of bmzzcr is
  signal ltrrs : integer;
  signal wt : severity_level;
  signal ml : real;
begin
  ueatnlp : entity work.mqwf
    port map (t => ml);
  mrtuenp : entity work.eppypf
    port map (zpm => wt, uoccepq => ltrrs, siry => idhjuu, jgcrmmvzmu => gggufpas);
  
  -- Multi-driven assignments
  qwx <= 'L';
end jc;



-- Seed after: 15557752564622064750,2230106469645304029
