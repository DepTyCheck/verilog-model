-- Seed: 4225371666352871186,13857275728440271305

library ieee;
use ieee.std_logic_1164.all;

entity cqwnr is
  port (pvgophprsu : inout std_logic_vector(2 downto 0); dpwus : buffer std_logic; ex : buffer real; ygt : buffer std_logic);
end cqwnr;

architecture vgov of cqwnr is
  
begin
  -- Single-driven assignments
  ex <= ex;
  
  -- Multi-driven assignments
  ygt <= '0';
  pvgophprsu <= "Z-1";
  ygt <= '-';
end vgov;

library ieee;
use ieee.std_logic_1164.all;

entity lfprzqrlzt is
  port (dpoye : buffer std_logic; v : in time; mvcozcd : inout integer; rpustjqpbc : inout integer);
end lfprzqrlzt;

library ieee;
use ieee.std_logic_1164.all;

architecture uxpypbtlg of lfprzqrlzt is
  signal dd : real;
  signal gxwaljvj : std_logic;
  signal mssd : std_logic_vector(2 downto 0);
  signal joafrzo : real;
  signal qogtw : std_logic;
  signal txcixcjxg : std_logic_vector(2 downto 0);
  signal elnfumacm : real;
  signal lxeiby : std_logic_vector(2 downto 0);
  signal jt : std_logic;
  signal qxwaquetd : real;
  signal xwq : std_logic_vector(2 downto 0);
begin
  txngwdb : entity work.cqwnr
    port map (pvgophprsu => xwq, dpwus => dpoye, ex => qxwaquetd, ygt => jt);
  gb : entity work.cqwnr
    port map (pvgophprsu => lxeiby, dpwus => dpoye, ex => elnfumacm, ygt => dpoye);
  rz : entity work.cqwnr
    port map (pvgophprsu => txcixcjxg, dpwus => qogtw, ex => joafrzo, ygt => qogtw);
  ubhqh : entity work.cqwnr
    port map (pvgophprsu => mssd, dpwus => gxwaljvj, ex => dd, ygt => qogtw);
  
  -- Single-driven assignments
  mvcozcd <= 0_2_2;
  rpustjqpbc <= rpustjqpbc;
  
  -- Multi-driven assignments
  mssd <= ('1', '-', 'U');
end uxpypbtlg;

entity mizglb is
  port (hone : buffer bit);
end mizglb;

library ieee;
use ieee.std_logic_1164.all;

architecture b of mizglb is
  signal jmhcneusz : real;
  signal rgvrhueqs : std_logic;
  signal uzdnxhkw : std_logic_vector(2 downto 0);
  signal tvck : integer;
  signal spdmgaov : integer;
  signal hky : integer;
  signal ynhwvgnwn : integer;
  signal pompbie : time;
  signal bqg : std_logic;
begin
  werigjns : entity work.lfprzqrlzt
    port map (dpoye => bqg, v => pompbie, mvcozcd => ynhwvgnwn, rpustjqpbc => hky);
  ewfatnhbup : entity work.lfprzqrlzt
    port map (dpoye => bqg, v => pompbie, mvcozcd => spdmgaov, rpustjqpbc => tvck);
  fzvqityg : entity work.cqwnr
    port map (pvgophprsu => uzdnxhkw, dpwus => rgvrhueqs, ex => jmhcneusz, ygt => bqg);
  
  -- Multi-driven assignments
  rgvrhueqs <= bqg;
end b;

entity btre is
  port (iiknz : inout integer_vector(1 downto 3));
end btre;

architecture mfvjtfuqt of btre is
  
begin
  
end mfvjtfuqt;



-- Seed after: 15368877735455470525,13857275728440271305
