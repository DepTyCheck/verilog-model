-- Seed: 9487850487007213180,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity r is
  port (phby : in real; f : inout std_logic_vector(4 downto 2); mgipdfdle : inout integer);
end r;

architecture dda of r is
  
begin
  -- Single-driven assignments
  mgipdfdle <= 1;
end dda;

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (jjfgio : buffer std_logic; mz : buffer real; hxsytmehrd : out time; ysobc : buffer time);
end d;

library ieee;
use ieee.std_logic_1164.all;

architecture bnpxavcgj of d is
  signal sxxyq : integer;
  signal fxowfm : std_logic_vector(4 downto 2);
  signal qlbda : integer;
  signal olllpvu : std_logic_vector(4 downto 2);
  signal wgmqxl : integer;
  signal nvqlpado : std_logic_vector(4 downto 2);
  signal ogutzhayd : real;
begin
  iw : entity work.r
    port map (phby => ogutzhayd, f => nvqlpado, mgipdfdle => wgmqxl);
  sftk : entity work.r
    port map (phby => mz, f => olllpvu, mgipdfdle => qlbda);
  raz : entity work.r
    port map (phby => ogutzhayd, f => fxowfm, mgipdfdle => sxxyq);
end bnpxavcgj;



-- Seed after: 15338145506267263630,16188444798499499427
