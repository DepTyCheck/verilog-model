-- Seed: 14842251061526596461,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity tbg is
  port (auq : linkage integer; guktyrhh : buffer std_logic_vector(4 to 0); r : out std_logic; fmbhgnbdon : in real);
end tbg;

architecture cdm of tbg is
  
begin
  -- Multi-driven assignments
  r <= '0';
end cdm;

entity lrjdn is
  port (wedsa : buffer integer_vector(1 downto 4); cka : buffer real);
end lrjdn;

library ieee;
use ieee.std_logic_1164.all;

architecture lnueovkp of lrjdn is
  signal zmw : integer;
  signal yglahiked : real;
  signal r : integer;
  signal skavia : real;
  signal az : std_logic;
  signal ecgmwsdr : integer;
  signal omwvvhcko : std_logic;
  signal yspcwszfj : std_logic_vector(4 to 0);
  signal zrk : integer;
begin
  dcwkdkni : entity work.tbg
    port map (auq => zrk, guktyrhh => yspcwszfj, r => omwvvhcko, fmbhgnbdon => cka);
  mixqdyucca : entity work.tbg
    port map (auq => ecgmwsdr, guktyrhh => yspcwszfj, r => az, fmbhgnbdon => skavia);
  zonlkcp : entity work.tbg
    port map (auq => r, guktyrhh => yspcwszfj, r => omwvvhcko, fmbhgnbdon => yglahiked);
  vjxtzyg : entity work.tbg
    port map (auq => zmw, guktyrhh => yspcwszfj, r => omwvvhcko, fmbhgnbdon => cka);
  
  -- Single-driven assignments
  skavia <= cka;
  wedsa <= wedsa;
  cka <= cka;
end lnueovkp;

entity igggce is
  port (wsmmu : linkage character);
end igggce;

library ieee;
use ieee.std_logic_1164.all;

architecture xupctsrtxo of igggce is
  signal wrthgxmzd : std_logic;
  signal kkqnjexoc : std_logic_vector(4 to 0);
  signal szvdm : integer;
  signal yug : real;
  signal hkb : integer_vector(1 downto 4);
begin
  xsdjts : entity work.lrjdn
    port map (wedsa => hkb, cka => yug);
  sjte : entity work.tbg
    port map (auq => szvdm, guktyrhh => kkqnjexoc, r => wrthgxmzd, fmbhgnbdon => yug);
  
  -- Multi-driven assignments
  wrthgxmzd <= wrthgxmzd;
  kkqnjexoc <= kkqnjexoc;
  kkqnjexoc <= kkqnjexoc;
  kkqnjexoc <= kkqnjexoc;
end xupctsrtxo;

entity f is
  port (xe : out time; uhulp : in time; iqtlxvm : out integer; ueiltick : inout time);
end f;

architecture vgblj of f is
  
begin
  -- Single-driven assignments
  ueiltick <= 2#0_0_0_1# ms;
end vgblj;



-- Seed after: 4245972431377373152,8068158652091157513
