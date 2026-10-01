-- Seed: 17752457443702482922,15025465285671019065

library ieee;
use ieee.std_logic_1164.all;

entity ybmzji is
  port (eo : buffer bit; dqyornzok : out std_logic; j : in std_logic_vector(2 to 1));
end ybmzji;

architecture iayvrglqad of ybmzji is
  
begin
  -- Single-driven assignments
  eo <= '0';
end iayvrglqad;

library ieee;
use ieee.std_logic_1164.all;

entity owjcjchvee is
  port (dhiq : in integer; xfzqnf : linkage time; ztcjsgbez : inout std_logic);
end owjcjchvee;

library ieee;
use ieee.std_logic_1164.all;

architecture jojiubqh of owjcjchvee is
  signal yczashmoa : bit;
  signal gqatuz : std_logic_vector(2 to 1);
  signal ijbl : std_logic;
  signal uzeqlxddva : bit;
begin
  cmajn : entity work.ybmzji
    port map (eo => uzeqlxddva, dqyornzok => ijbl, j => gqatuz);
  nm : entity work.ybmzji
    port map (eo => yczashmoa, dqyornzok => ztcjsgbez, j => gqatuz);
  
  -- Multi-driven assignments
  ztcjsgbez <= 'W';
  gqatuz <= (others => '0');
end jojiubqh;

entity vaxthut is
  port (xfsojxq : linkage integer);
end vaxthut;

library ieee;
use ieee.std_logic_1164.all;

architecture yxnxkefuge of vaxthut is
  signal lfjhxdexs : std_logic_vector(2 to 1);
  signal n : bit;
  signal i : std_logic_vector(2 to 1);
  signal iezchxekgl : std_logic;
  signal nluagi : bit;
  signal atgfw : std_logic_vector(2 to 1);
  signal lzfbkjbx : std_logic;
  signal piozczel : bit;
begin
  zpfaka : entity work.ybmzji
    port map (eo => piozczel, dqyornzok => lzfbkjbx, j => atgfw);
  gbvjuyq : entity work.ybmzji
    port map (eo => nluagi, dqyornzok => iezchxekgl, j => i);
  vljo : entity work.ybmzji
    port map (eo => n, dqyornzok => lzfbkjbx, j => lfjhxdexs);
  
  -- Multi-driven assignments
  atgfw <= "";
end yxnxkefuge;

entity uehdpdskfn is
  port (kziiarfi : inout boolean_vector(3 downto 1); eir : linkage time);
end uehdpdskfn;

library ieee;
use ieee.std_logic_1164.all;

architecture rnko of uehdpdskfn is
  signal hsswjimyn : std_logic;
  signal junj : time;
  signal opwiddoct : integer;
  signal vf : std_logic_vector(2 to 1);
  signal sotiotl : bit;
  signal oslb : std_logic;
  signal iahbcwaq : time;
  signal wcakdfsqh : integer;
begin
  yer : entity work.owjcjchvee
    port map (dhiq => wcakdfsqh, xfzqnf => iahbcwaq, ztcjsgbez => oslb);
  adzktbaiwz : entity work.ybmzji
    port map (eo => sotiotl, dqyornzok => oslb, j => vf);
  ipigovbrp : entity work.owjcjchvee
    port map (dhiq => opwiddoct, xfzqnf => junj, ztcjsgbez => hsswjimyn);
  
  -- Multi-driven assignments
  oslb <= 'W';
end rnko;



-- Seed after: 4727212027650522328,15025465285671019065
