-- Seed: 144299006450881378,511364357853360275

entity bpbs is
  port (yhgo : out time; sovogli : in bit_vector(0 downto 4));
end bpbs;

architecture s of bpbs is
  
begin
  -- Single-driven assignments
  yhgo <= 0_3_0_4 ms;
end s;

entity e is
  port (dcr : linkage boolean; mgymdzhc : linkage severity_level);
end e;

architecture pqbcvoy of e is
  
begin
  
end pqbcvoy;

library ieee;
use ieee.std_logic_1164.all;

entity ut is
  port (gsthgixudw : linkage time; ki : in integer; rlwrdsm : buffer std_logic; r : in std_logic);
end ut;

architecture bwksayryh of ut is
  signal tspqixdwb : severity_level;
  signal jjfjjhtol : boolean;
  signal ejweppdw : bit_vector(0 downto 4);
  signal oa : time;
begin
  lnbncb : entity work.bpbs
    port map (yhgo => oa, sovogli => ejweppdw);
  kh : entity work.e
    port map (dcr => jjfjjhtol, mgymdzhc => tspqixdwb);
  
  -- Single-driven assignments
  ejweppdw <= (others => '0');
end bwksayryh;

entity lu is
  port (vkgslunroi : out time);
end lu;

architecture fprzz of lu is
  signal ekqvsvcw : bit_vector(0 downto 4);
  signal cke : time;
  signal cthh : bit_vector(0 downto 4);
  signal gewtrg : bit_vector(0 downto 4);
  signal uyvyhmqjgd : time;
begin
  zpsclamifm : entity work.bpbs
    port map (yhgo => uyvyhmqjgd, sovogli => gewtrg);
  bsmjoxhb : entity work.bpbs
    port map (yhgo => vkgslunroi, sovogli => cthh);
  otllj : entity work.bpbs
    port map (yhgo => cke, sovogli => ekqvsvcw);
end fprzz;



-- Seed after: 7404005436478390219,511364357853360275
