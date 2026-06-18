-- Seed: 13383412871849511800,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity svecpc is
  port (e : buffer std_logic_vector(2 to 4); ladi : inout std_logic);
end svecpc;

architecture xh of svecpc is
  
begin
  -- Multi-driven assignments
  e <= ('0', 'W', 'U');
  ladi <= '1';
  ladi <= 'U';
end xh;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (lgu : in real_vector(4 to 0); dihkczcul : linkage std_logic_vector(0 to 4); tzol : inout time);
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture lbypzeghrc of a is
  signal wwyqpt : std_logic;
  signal rvkkqhgsqp : std_logic;
  signal xljqjuq : std_logic_vector(2 to 4);
begin
  gy : entity work.svecpc
    port map (e => xljqjuq, ladi => rvkkqhgsqp);
  qnhzuu : entity work.svecpc
    port map (e => xljqjuq, ladi => wwyqpt);
  nxihu : entity work.svecpc
    port map (e => xljqjuq, ladi => rvkkqhgsqp);
  
  -- Single-driven assignments
  tzol <= 2 sec;
  
  -- Multi-driven assignments
  xljqjuq <= "1UL";
  xljqjuq <= "WZL";
  xljqjuq <= ('L', 'H', 'H');
  wwyqpt <= '-';
end lbypzeghrc;

library ieee;
use ieee.std_logic_1164.all;

entity vtuzjpsneg is
  port (nahntz : buffer std_logic; ndp : linkage real; u : out bit);
end vtuzjpsneg;

library ieee;
use ieee.std_logic_1164.all;

architecture bbfexgdkag of vtuzjpsneg is
  signal dlgfv : std_logic;
  signal yff : std_logic_vector(2 to 4);
  signal qahpj : time;
  signal maroyi : std_logic_vector(0 to 4);
  signal fhlww : real_vector(4 to 0);
begin
  vqi : entity work.a
    port map (lgu => fhlww, dihkczcul => maroyi, tzol => qahpj);
  uampr : entity work.svecpc
    port map (e => yff, ladi => dlgfv);
  
  -- Single-driven assignments
  u <= '0';
  fhlww <= (others => 0.0);
  
  -- Multi-driven assignments
  nahntz <= '-';
  nahntz <= 'W';
  yff <= "U1W";
  maroyi <= ('L', 'U', 'H', '0', 'L');
end bbfexgdkag;



-- Seed after: 13573676862147727567,8118127366649987907
