-- Seed: 3469483028087420669,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity whzg is
  port (vebfeyq : inout std_logic; thpduxf : buffer time; rdh : inout std_logic_vector(4 to 2); jfrqrq : buffer std_logic_vector(0 to 2));
end whzg;

architecture dvfna of whzg is
  
begin
  -- Single-driven assignments
  thpduxf <= 3_4_0_2.2403 fs;
end dvfna;

library ieee;
use ieee.std_logic_1164.all;

entity gwenbcknpn is
  port (ph : in severity_level; nzk : buffer std_logic_vector(4 downto 3); ugiqnp : buffer time);
end gwenbcknpn;

library ieee;
use ieee.std_logic_1164.all;

architecture mrlbqk of gwenbcknpn is
  signal dgqslux : std_logic;
  signal rik : std_logic_vector(4 to 2);
  signal pg : time;
  signal oi : std_logic;
  signal pfpy : std_logic_vector(0 to 2);
  signal nsxtktyo : time;
  signal cqspcdqvre : std_logic_vector(0 to 2);
  signal tt : std_logic_vector(4 to 2);
  signal qlasyjyli : time;
  signal xphlkbupzk : std_logic;
begin
  usqeuklct : entity work.whzg
    port map (vebfeyq => xphlkbupzk, thpduxf => qlasyjyli, rdh => tt, jfrqrq => cqspcdqvre);
  duusagd : entity work.whzg
    port map (vebfeyq => xphlkbupzk, thpduxf => nsxtktyo, rdh => tt, jfrqrq => pfpy);
  kowa : entity work.whzg
    port map (vebfeyq => oi, thpduxf => pg, rdh => rik, jfrqrq => cqspcdqvre);
  w : entity work.whzg
    port map (vebfeyq => dgqslux, thpduxf => ugiqnp, rdh => tt, jfrqrq => cqspcdqvre);
  
  -- Multi-driven assignments
  oi <= 'L';
  tt <= "";
  dgqslux <= 'W';
  dgqslux <= '1';
end mrlbqk;

library ieee;
use ieee.std_logic_1164.all;

entity tjbqfhmg is
  port (qv : linkage boolean; dygzcic : in std_logic; oc : linkage boolean_vector(0 to 1));
end tjbqfhmg;

library ieee;
use ieee.std_logic_1164.all;

architecture tdabgytko of tjbqfhmg is
  signal gc : std_logic_vector(0 to 2);
  signal pqio : std_logic_vector(4 to 2);
  signal ehbkloe : time;
  signal kx : std_logic;
  signal pyb : time;
  signal z : std_logic_vector(4 downto 3);
  signal vrdkoo : severity_level;
begin
  cegozimqe : entity work.gwenbcknpn
    port map (ph => vrdkoo, nzk => z, ugiqnp => pyb);
  li : entity work.whzg
    port map (vebfeyq => kx, thpduxf => ehbkloe, rdh => pqio, jfrqrq => gc);
  
  -- Single-driven assignments
  vrdkoo <= ERROR;
end tdabgytko;

entity qnst is
  port (xowrvjkq : in integer; qwc : linkage time; wavqhuwrk : in time);
end qnst;

library ieee;
use ieee.std_logic_1164.all;

architecture bmzajzcmq of qnst is
  signal nkqvz : time;
  signal kabbmeya : std_logic_vector(4 downto 3);
  signal e : severity_level;
  signal jmji : std_logic_vector(0 to 2);
  signal h : std_logic_vector(4 to 2);
  signal ncap : time;
  signal ismxhpjfz : std_logic;
begin
  qqwuotj : entity work.whzg
    port map (vebfeyq => ismxhpjfz, thpduxf => ncap, rdh => h, jfrqrq => jmji);
  dxjmv : entity work.gwenbcknpn
    port map (ph => e, nzk => kabbmeya, ugiqnp => nkqvz);
  
  -- Single-driven assignments
  e <= WARNING;
  
  -- Multi-driven assignments
  ismxhpjfz <= '1';
  h <= (others => '0');
  ismxhpjfz <= 'L';
  ismxhpjfz <= 'L';
end bmzajzcmq;



-- Seed after: 3188854304370173133,3924983747739634027
