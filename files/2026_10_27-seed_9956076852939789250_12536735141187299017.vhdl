-- Seed: 9956076852939789250,12536735141187299017

entity niuflwxfwl is
  port (c : out time; wjrpuvt : out real; uk : in bit_vector(1 downto 0));
end niuflwxfwl;

architecture mhtprfqzw of niuflwxfwl is
  
begin
  -- Single-driven assignments
  wjrpuvt <= wjrpuvt;
  c <= c;
end mhtprfqzw;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (gthsilo : inout std_logic; hkqlokar : in boolean_vector(4 to 2); lp : linkage real);
end q;

architecture puc of q is
  signal yzotfiz : real;
  signal mdi : time;
  signal dirsmdbmr : bit_vector(1 downto 0);
  signal gweqwou : real;
  signal zvcy : time;
begin
  vrixnuqi : entity work.niuflwxfwl
    port map (c => zvcy, wjrpuvt => gweqwou, uk => dirsmdbmr);
  w : entity work.niuflwxfwl
    port map (c => mdi, wjrpuvt => yzotfiz, uk => dirsmdbmr);
  
  -- Single-driven assignments
  dirsmdbmr <= ('0', '1');
  
  -- Multi-driven assignments
  gthsilo <= 'W';
end puc;

library ieee;
use ieee.std_logic_1164.all;

entity gmjl is
  port (vuywb : buffer std_logic; ebaeudmrpj : in character);
end gmjl;

architecture yjhlexj of gmjl is
  
begin
  -- Multi-driven assignments
  vuywb <= '-';
  vuywb <= '1';
  vuywb <= 'H';
end yjhlexj;

entity rz is
  port (awz : linkage bit);
end rz;

library ieee;
use ieee.std_logic_1164.all;

architecture fjjddlzd of rz is
  signal dpvpxznu : bit_vector(1 downto 0);
  signal skoontd : real;
  signal pz : time;
  signal oneubw : bit_vector(1 downto 0);
  signal dyu : real;
  signal aeqilrcp : time;
  signal px : character;
  signal g : real;
  signal o : boolean_vector(4 to 2);
  signal kcxb : std_logic;
begin
  uzhscyny : entity work.q
    port map (gthsilo => kcxb, hkqlokar => o, lp => g);
  czgtce : entity work.gmjl
    port map (vuywb => kcxb, ebaeudmrpj => px);
  hpgjucmdiu : entity work.niuflwxfwl
    port map (c => aeqilrcp, wjrpuvt => dyu, uk => oneubw);
  nschuo : entity work.niuflwxfwl
    port map (c => pz, wjrpuvt => skoontd, uk => dpvpxznu);
  
  -- Single-driven assignments
  dpvpxznu <= ('1', '1');
  px <= 'd';
  o <= (others => TRUE);
  oneubw <= ('0', '1');
  
  -- Multi-driven assignments
  kcxb <= kcxb;
  kcxb <= 'W';
end fjjddlzd;



-- Seed after: 5118186105782732285,12536735141187299017
