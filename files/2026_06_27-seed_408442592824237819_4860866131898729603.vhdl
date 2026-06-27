-- Seed: 408442592824237819,4860866131898729603

entity s is
  port (wbng : inout integer; rxxnd : out integer);
end s;

architecture v of s is
  
begin
  -- Single-driven assignments
  rxxnd <= 16#9#;
  wbng <= 16#7E#;
end v;

library ieee;
use ieee.std_logic_1164.all;

entity sophd is
  port (jmwzuujfyx : inout std_logic_vector(4 downto 2); n : linkage real);
end sophd;

architecture hcsg of sophd is
  signal bfwtfbgki : integer;
  signal jyh : integer;
  signal cgyku : integer;
  signal vkdf : integer;
  signal dgrhhnetq : integer;
  signal xzyrfqe : integer;
  signal xppsyawf : integer;
  signal qdyzjdis : integer;
begin
  sa : entity work.s
    port map (wbng => qdyzjdis, rxxnd => xppsyawf);
  ostsyxya : entity work.s
    port map (wbng => xzyrfqe, rxxnd => dgrhhnetq);
  nctgdnunm : entity work.s
    port map (wbng => vkdf, rxxnd => cgyku);
  aisvinppn : entity work.s
    port map (wbng => jyh, rxxnd => bfwtfbgki);
  
  -- Multi-driven assignments
  jmwzuujfyx <= "H00";
  jmwzuujfyx <= ('W', '0', 'X');
  jmwzuujfyx <= ('1', 'U', '1');
  jmwzuujfyx <= "L10";
end hcsg;



-- Seed after: 7092944005055019928,4860866131898729603
