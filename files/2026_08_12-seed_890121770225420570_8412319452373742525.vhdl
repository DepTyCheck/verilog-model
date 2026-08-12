-- Seed: 890121770225420570,8412319452373742525

entity u is
  port (dvdiwrb : inout time_vector(4 downto 1));
end u;

architecture wjc of u is
  
begin
  -- Single-driven assignments
  dvdiwrb <= (31 fs, 16#6_6_C# ms, 16#D_F_2# ps, 2#01001.10011# us);
end wjc;

library ieee;
use ieee.std_logic_1164.all;

entity dstrjnau is
  port (tlimzrzlt : inout real; lyfygu : buffer std_logic_vector(4 to 2); qoxqiux : in std_logic_vector(3 to 3));
end dstrjnau;

architecture wkoo of dstrjnau is
  signal rrhcpd : time_vector(4 downto 1);
  signal h : time_vector(4 downto 1);
  signal jhdz : time_vector(4 downto 1);
begin
  zokrn : entity work.u
    port map (dvdiwrb => jhdz);
  w : entity work.u
    port map (dvdiwrb => h);
  abtmhchh : entity work.u
    port map (dvdiwrb => rrhcpd);
end wkoo;



-- Seed after: 4438963451013806967,8412319452373742525
