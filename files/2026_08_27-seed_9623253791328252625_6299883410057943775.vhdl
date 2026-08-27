-- Seed: 9623253791328252625,6299883410057943775

library ieee;
use ieee.std_logic_1164.all;

entity fmvsax is
  port (hrlnuk : inout std_logic; tpzaas : buffer time; ciwmvsfgey : linkage std_logic; rnqz : buffer integer_vector(2 to 2));
end fmvsax;

architecture t of fmvsax is
  
begin
  -- Single-driven assignments
  tpzaas <= 1 hr;
end t;

entity vk is
  port (ijmertih : in severity_level);
end vk;

library ieee;
use ieee.std_logic_1164.all;

architecture tozhexewu of vk is
  signal tmcc : integer_vector(2 to 2);
  signal wqk : time;
  signal dnke : std_logic;
begin
  cynrhwvpg : entity work.fmvsax
    port map (hrlnuk => dnke, tpzaas => wqk, ciwmvsfgey => dnke, rnqz => tmcc);
  
  -- Multi-driven assignments
  dnke <= dnke;
  dnke <= 'X';
  dnke <= dnke;
end tozhexewu;

entity lcbwunqdn is
  port (a : buffer bit_vector(4 downto 0); bpdznyyk : buffer real; mblgskl : out severity_level);
end lcbwunqdn;

library ieee;
use ieee.std_logic_1164.all;

architecture we of lcbwunqdn is
  signal jwrvkhlgj : integer_vector(2 to 2);
  signal h : std_logic;
  signal gxwivpo : time;
  signal hwdasmv : integer_vector(2 to 2);
  signal grj : time;
  signal pgatjxhps : std_logic;
  signal pterwsezei : severity_level;
begin
  jaaejldp : entity work.vk
    port map (ijmertih => pterwsezei);
  dlgylyk : entity work.fmvsax
    port map (hrlnuk => pgatjxhps, tpzaas => grj, ciwmvsfgey => pgatjxhps, rnqz => hwdasmv);
  xpbwgysvo : entity work.vk
    port map (ijmertih => mblgskl);
  tezhtftmzg : entity work.fmvsax
    port map (hrlnuk => pgatjxhps, tpzaas => gxwivpo, ciwmvsfgey => h, rnqz => jwrvkhlgj);
  
  -- Single-driven assignments
  a <= ('0', '0', '1', '0', '1');
  pterwsezei <= mblgskl;
  mblgskl <= NOTE;
  bpdznyyk <= bpdznyyk;
  
  -- Multi-driven assignments
  pgatjxhps <= pgatjxhps;
  pgatjxhps <= pgatjxhps;
  pgatjxhps <= '0';
  pgatjxhps <= pgatjxhps;
end we;



-- Seed after: 3225509598392132027,6299883410057943775
