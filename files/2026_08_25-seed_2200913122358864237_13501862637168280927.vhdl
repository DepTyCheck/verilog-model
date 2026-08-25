-- Seed: 2200913122358864237,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity lfioqluev is
  port (mujqlee : linkage std_logic_vector(4 to 2); h : out severity_level; awcwx : linkage std_logic; tr : out integer);
end lfioqluev;

architecture cep of lfioqluev is
  
begin
  -- Single-driven assignments
  h <= FAILURE;
  tr <= 1;
end cep;

library ieee;
use ieee.std_logic_1164.all;

entity fw is
  port (dok : linkage std_logic_vector(4 downto 4); ujnjkxvjp : in std_logic; inocxuhdet : out real; zcnka : inout bit);
end fw;

library ieee;
use ieee.std_logic_1164.all;

architecture ifkrflx of fw is
  signal xkdsgh : integer;
  signal htkvfi : std_logic;
  signal nhxeyb : severity_level;
  signal bldcuq : std_logic_vector(4 to 2);
  signal vzl : integer;
  signal oxtdpz : std_logic;
  signal gnwdug : severity_level;
  signal mpq : std_logic_vector(4 to 2);
begin
  dx : entity work.lfioqluev
    port map (mujqlee => mpq, h => gnwdug, awcwx => oxtdpz, tr => vzl);
  ooxotecs : entity work.lfioqluev
    port map (mujqlee => bldcuq, h => nhxeyb, awcwx => htkvfi, tr => xkdsgh);
  
  -- Multi-driven assignments
  mpq <= (others => '0');
  mpq <= bldcuq;
end ifkrflx;

entity ycdvrglit is
  port (k : linkage integer);
end ycdvrglit;

library ieee;
use ieee.std_logic_1164.all;

architecture kydtlletm of ycdvrglit is
  signal m : integer;
  signal y : severity_level;
  signal gwiielqtx : std_logic_vector(4 to 2);
  signal vmsywfzoh : integer;
  signal rknqzpfrxc : std_logic;
  signal clajyzsgz : severity_level;
  signal ntxf : integer;
  signal yjtwb : severity_level;
  signal cmlfim : std_logic_vector(4 to 2);
  signal vfpefyzgxp : bit;
  signal piaa : real;
  signal mwztdgpnw : std_logic;
  signal qa : std_logic_vector(4 downto 4);
begin
  xsbmgox : entity work.fw
    port map (dok => qa, ujnjkxvjp => mwztdgpnw, inocxuhdet => piaa, zcnka => vfpefyzgxp);
  jd : entity work.lfioqluev
    port map (mujqlee => cmlfim, h => yjtwb, awcwx => mwztdgpnw, tr => ntxf);
  ak : entity work.lfioqluev
    port map (mujqlee => cmlfim, h => clajyzsgz, awcwx => rknqzpfrxc, tr => vmsywfzoh);
  rqpnosemfa : entity work.lfioqluev
    port map (mujqlee => gwiielqtx, h => y, awcwx => rknqzpfrxc, tr => m);
  
  -- Multi-driven assignments
  rknqzpfrxc <= mwztdgpnw;
  qa <= qa;
  qa <= "W";
  mwztdgpnw <= '0';
end kydtlletm;



-- Seed after: 7672177623269479999,13501862637168280927
