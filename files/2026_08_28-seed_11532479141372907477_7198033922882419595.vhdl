-- Seed: 11532479141372907477,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity uzgdddr is
  port (lemk : inout integer_vector(2 downto 3); tbnh : in std_logic);
end uzgdddr;

architecture bse of uzgdddr is
  
begin
  -- Single-driven assignments
  lemk <= (others => 0);
end bse;

entity pufjxdp is
  port (nfbo : linkage time);
end pufjxdp;

library ieee;
use ieee.std_logic_1164.all;

architecture vodmvnqjti of pufjxdp is
  signal ztinhggld : std_logic;
  signal whkecb : integer_vector(2 downto 3);
begin
  w : entity work.uzgdddr
    port map (lemk => whkecb, tbnh => ztinhggld);
  
  -- Multi-driven assignments
  ztinhggld <= ztinhggld;
  ztinhggld <= 'W';
  ztinhggld <= ztinhggld;
  ztinhggld <= 'X';
end vodmvnqjti;

entity se is
  port (iedbma : linkage integer_vector(3 downto 0); vmtmiwby : linkage string(2 to 1));
end se;

library ieee;
use ieee.std_logic_1164.all;

architecture jwiccf of se is
  signal fwfqsqv : time;
  signal oewin : time;
  signal cgtlr : std_logic;
  signal gi : integer_vector(2 downto 3);
begin
  viwspzs : entity work.uzgdddr
    port map (lemk => gi, tbnh => cgtlr);
  rghq : entity work.pufjxdp
    port map (nfbo => oewin);
  kvzrqb : entity work.pufjxdp
    port map (nfbo => fwfqsqv);
  
  -- Multi-driven assignments
  cgtlr <= 'H';
  cgtlr <= 'U';
  cgtlr <= cgtlr;
end jwiccf;



-- Seed after: 4925199186114528044,7198033922882419595
