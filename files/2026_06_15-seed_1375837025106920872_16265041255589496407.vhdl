-- Seed: 1375837025106920872,16265041255589496407

library ieee;
use ieee.std_logic_1164.all;

entity nvfjpv is
  port (hwhmtdujbo : inout std_logic_vector(2 to 4); atldd : out boolean_vector(4 downto 3); p : in time);
end nvfjpv;



architecture wjg of nvfjpv is
  
begin
  
end wjg;



entity fsx is
  port (bra : linkage real; zstaf : inout boolean; japdgo : inout real);
end fsx;

library ieee;
use ieee.std_logic_1164.all;

architecture aeqf of fsx is
  signal axacjcjoxk : time;
  signal lss : boolean_vector(4 downto 3);
  signal g : std_logic_vector(2 to 4);
  signal w : time;
  signal x : boolean_vector(4 downto 3);
  signal r : std_logic_vector(2 to 4);
  signal poia : time;
  signal sgba : boolean_vector(4 downto 3);
  signal h : std_logic_vector(2 to 4);
  signal sefadmzjzn : time;
  signal qxwfyipt : boolean_vector(4 downto 3);
  signal tupoxdqekl : std_logic_vector(2 to 4);
begin
  ckgs : entity work.nvfjpv
    port map (hwhmtdujbo => tupoxdqekl, atldd => qxwfyipt, p => sefadmzjzn);
  erw : entity work.nvfjpv
    port map (hwhmtdujbo => h, atldd => sgba, p => poia);
  spwh : entity work.nvfjpv
    port map (hwhmtdujbo => r, atldd => x, p => w);
  ssokfyr : entity work.nvfjpv
    port map (hwhmtdujbo => g, atldd => lss, p => axacjcjoxk);
end aeqf;



entity ivvwpvqp is
  port (jx : inout integer);
end ivvwpvqp;

library ieee;
use ieee.std_logic_1164.all;

architecture ezzbfjhddz of ivvwpvqp is
  signal szaufcjb : time;
  signal slwyrmohn : boolean_vector(4 downto 3);
  signal elr : real;
  signal kuzt : boolean;
  signal xzuennuka : real;
  signal uyhinetf : time;
  signal bou : boolean_vector(4 downto 3);
  signal vdvlfjvhet : std_logic_vector(2 to 4);
begin
  rzw : entity work.nvfjpv
    port map (hwhmtdujbo => vdvlfjvhet, atldd => bou, p => uyhinetf);
  reyormkwf : entity work.fsx
    port map (bra => xzuennuka, zstaf => kuzt, japdgo => elr);
  hwzy : entity work.nvfjpv
    port map (hwhmtdujbo => vdvlfjvhet, atldd => slwyrmohn, p => szaufcjb);
end ezzbfjhddz;

library ieee;
use ieee.std_logic_1164.all;

entity hxr is
  port (ogiscunud : buffer std_logic_vector(3 to 1); yda : linkage severity_level; t : out integer; h : linkage boolean_vector(2 downto 1));
end hxr;

library ieee;
use ieee.std_logic_1164.all;

architecture lcljlt of hxr is
  signal xon : integer;
  signal pfj : boolean_vector(4 downto 3);
  signal bp : std_logic_vector(2 to 4);
  signal jcynfc : time;
  signal ha : boolean_vector(4 downto 3);
  signal lmlsfvvtav : std_logic_vector(2 to 4);
begin
  jfz : entity work.nvfjpv
    port map (hwhmtdujbo => lmlsfvvtav, atldd => ha, p => jcynfc);
  zaqsqvmcx : entity work.nvfjpv
    port map (hwhmtdujbo => bp, atldd => pfj, p => jcynfc);
  tplq : entity work.ivvwpvqp
    port map (jx => xon);
  j : entity work.ivvwpvqp
    port map (jx => t);
end lcljlt;



-- Seed after: 9904373237401389127,16265041255589496407
