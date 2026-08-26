-- Seed: 15530079306571220486,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity xnn is
  port (fqqowbkofi : in std_logic; zsfuq : buffer std_logic_vector(4 downto 2); jqozfog : inout real; mfwx : in std_logic_vector(0 to 3));
end xnn;

architecture sritep of xnn is
  
begin
  -- Single-driven assignments
  jqozfog <= jqozfog;
end sritep;

entity tahipnt is
  port (oa : buffer time);
end tahipnt;

library ieee;
use ieee.std_logic_1164.all;

architecture zmjhvggsy of tahipnt is
  signal k : real;
  signal vriz : std_logic_vector(4 downto 2);
  signal no : std_logic;
  signal fqh : real;
  signal ewlhrxuzc : std_logic;
  signal zpxhwtaim : std_logic_vector(0 to 3);
  signal jqlvifikl : real;
  signal ut : std_logic_vector(4 downto 2);
  signal pkv : std_logic;
  signal zdyygckwd : std_logic_vector(0 to 3);
  signal cqh : real;
  signal aiob : std_logic_vector(4 downto 2);
  signal sshe : std_logic;
begin
  npbcelffdo : entity work.xnn
    port map (fqqowbkofi => sshe, zsfuq => aiob, jqozfog => cqh, mfwx => zdyygckwd);
  bwmlcf : entity work.xnn
    port map (fqqowbkofi => pkv, zsfuq => ut, jqozfog => jqlvifikl, mfwx => zpxhwtaim);
  lsoxcdoz : entity work.xnn
    port map (fqqowbkofi => ewlhrxuzc, zsfuq => aiob, jqozfog => fqh, mfwx => zdyygckwd);
  qlpfnkuto : entity work.xnn
    port map (fqqowbkofi => no, zsfuq => vriz, jqozfog => k, mfwx => zpxhwtaim);
  
  -- Single-driven assignments
  oa <= 2#0.0_0_1# ns;
  
  -- Multi-driven assignments
  sshe <= sshe;
  zdyygckwd <= zdyygckwd;
  pkv <= 'U';
  no <= no;
end zmjhvggsy;

entity utxsgkjl is
  port (nkz : inout time);
end utxsgkjl;

library ieee;
use ieee.std_logic_1164.all;

architecture p of utxsgkjl is
  signal cx : std_logic_vector(0 to 3);
  signal wzzdd : real;
  signal aany : real;
  signal li : std_logic_vector(0 to 3);
  signal irwblwz : real;
  signal hfuazr : std_logic_vector(4 downto 2);
  signal xrtqazb : std_logic;
begin
  jgkaxvab : entity work.tahipnt
    port map (oa => nkz);
  a : entity work.xnn
    port map (fqqowbkofi => xrtqazb, zsfuq => hfuazr, jqozfog => irwblwz, mfwx => li);
  djgtdo : entity work.xnn
    port map (fqqowbkofi => xrtqazb, zsfuq => hfuazr, jqozfog => aany, mfwx => li);
  vyf : entity work.xnn
    port map (fqqowbkofi => xrtqazb, zsfuq => hfuazr, jqozfog => wzzdd, mfwx => cx);
  
  -- Multi-driven assignments
  xrtqazb <= 'X';
  xrtqazb <= 'X';
  xrtqazb <= xrtqazb;
  xrtqazb <= xrtqazb;
end p;



-- Seed after: 8110479335181287114,6000118208082478503
