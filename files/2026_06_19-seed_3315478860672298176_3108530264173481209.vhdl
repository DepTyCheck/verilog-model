-- Seed: 3315478860672298176,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity dcpmlcno is
  port (mdnal : out std_logic_vector(1 downto 2); dfcuaw : buffer std_logic_vector(1 to 4); hacztpfq : out std_logic_vector(0 to 1));
end dcpmlcno;

architecture jyvn of dcpmlcno is
  
begin
  
end jyvn;

entity tgpgugfnc is
  port (wh : inout real; qmp : buffer integer; rxfnukulx : linkage integer);
end tgpgugfnc;

library ieee;
use ieee.std_logic_1164.all;

architecture bbbrm of tgpgugfnc is
  signal qsbzvqrok : std_logic_vector(1 to 4);
  signal mtjdsj : std_logic_vector(1 downto 2);
  signal xilukvzrx : std_logic_vector(0 to 1);
  signal jviev : std_logic_vector(1 to 4);
  signal tuwwpbptc : std_logic_vector(1 downto 2);
begin
  ehzl : entity work.dcpmlcno
    port map (mdnal => tuwwpbptc, dfcuaw => jviev, hacztpfq => xilukvzrx);
  niszfli : entity work.dcpmlcno
    port map (mdnal => mtjdsj, dfcuaw => qsbzvqrok, hacztpfq => xilukvzrx);
  
  -- Single-driven assignments
  qmp <= 40;
  wh <= 1_1_3.1_1;
  
  -- Multi-driven assignments
  tuwwpbptc <= "";
  mtjdsj <= "";
end bbbrm;

library ieee;
use ieee.std_logic_1164.all;

entity botgbvmfl is
  port (gj : in std_logic; ew : out integer; vojwm : inout time);
end botgbvmfl;

library ieee;
use ieee.std_logic_1164.all;

architecture rf of botgbvmfl is
  signal zjinjef : std_logic_vector(0 to 1);
  signal amdovfgiy : std_logic_vector(1 to 4);
  signal gvxxlkjr : std_logic_vector(1 downto 2);
begin
  bu : entity work.dcpmlcno
    port map (mdnal => gvxxlkjr, dfcuaw => amdovfgiy, hacztpfq => zjinjef);
  
  -- Single-driven assignments
  vojwm <= 1104 ns;
  ew <= 2#0_0_0#;
  
  -- Multi-driven assignments
  gvxxlkjr <= (others => '0');
end rf;

library ieee;
use ieee.std_logic_1164.all;

entity lseuc is
  port (tkqxumiw : buffer bit; dbtwint : linkage std_logic; ghdzofywec : linkage boolean; bsb : linkage real);
end lseuc;

library ieee;
use ieee.std_logic_1164.all;

architecture xw of lseuc is
  signal u : std_logic_vector(1 to 4);
  signal sumi : std_logic_vector(1 downto 2);
  signal dclgnjjt : std_logic_vector(0 to 1);
  signal rpfq : std_logic_vector(1 to 4);
  signal npubvaa : std_logic_vector(1 downto 2);
begin
  d : entity work.dcpmlcno
    port map (mdnal => npubvaa, dfcuaw => rpfq, hacztpfq => dclgnjjt);
  vhbm : entity work.dcpmlcno
    port map (mdnal => sumi, dfcuaw => rpfq, hacztpfq => dclgnjjt);
  ivpj : entity work.dcpmlcno
    port map (mdnal => sumi, dfcuaw => u, hacztpfq => dclgnjjt);
  
  -- Single-driven assignments
  tkqxumiw <= '1';
  
  -- Multi-driven assignments
  rpfq <= ('L', 'L', 'U', 'U');
  u <= ('-', 'W', 'W', 'Z');
  npubvaa <= (others => '0');
end xw;



-- Seed after: 7794430564336521907,3108530264173481209
