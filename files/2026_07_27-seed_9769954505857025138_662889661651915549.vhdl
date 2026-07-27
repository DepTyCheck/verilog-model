-- Seed: 9769954505857025138,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity qj is
  port (fqfts : inout time; b : buffer boolean_vector(3 to 1); mxvz : inout integer; qizo : inout std_logic_vector(2 to 4));
end qj;

architecture orvdm of qj is
  
begin
  -- Single-driven assignments
  mxvz <= 2#0_0_0_0#;
  b <= (others => TRUE);
end orvdm;

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (iftemygidd : in std_logic_vector(2 downto 2); xioytyruja : buffer integer; x : out std_logic_vector(1 downto 3); pmipnqz : in integer);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture gnxlgfh of r is
  signal fkdqe : integer;
  signal ndug : boolean_vector(3 to 1);
  signal b : time;
  signal co : std_logic_vector(2 to 4);
  signal h : boolean_vector(3 to 1);
  signal blnakzge : time;
begin
  jchwofss : entity work.qj
    port map (fqfts => blnakzge, b => h, mxvz => xioytyruja, qizo => co);
  olfgm : entity work.qj
    port map (fqfts => b, b => ndug, mxvz => fkdqe, qizo => co);
  
  -- Multi-driven assignments
  co <= co;
  x <= x;
end gnxlgfh;



-- Seed after: 8370987599478829498,662889661651915549
