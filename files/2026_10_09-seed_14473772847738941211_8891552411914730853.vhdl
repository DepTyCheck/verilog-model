-- Seed: 14473772847738941211,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity gqxlpf is
  port (jdzowq : in integer; bpaskl : in std_logic_vector(2 to 4); bbyv : buffer boolean_vector(2 downto 3); iqgpskyasy : inout real);
end gqxlpf;

architecture uutq of gqxlpf is
  
begin
  
end uutq;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (hldwz : out real; qhrv : buffer std_logic; utxyg : buffer std_logic_vector(4 downto 3); fctf : buffer severity_level);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture xtlnlf of f is
  signal mjandvjux : real;
  signal ebvs : boolean_vector(2 downto 3);
  signal cb : std_logic_vector(2 to 4);
  signal pkiz : integer;
begin
  pakcs : entity work.gqxlpf
    port map (jdzowq => pkiz, bpaskl => cb, bbyv => ebvs, iqgpskyasy => mjandvjux);
  
  -- Single-driven assignments
  pkiz <= pkiz;
  fctf <= fctf;
  hldwz <= mjandvjux;
  
  -- Multi-driven assignments
  qhrv <= 'X';
  utxyg <= ('W', '1');
end xtlnlf;



-- Seed after: 4232337116795384072,8891552411914730853
