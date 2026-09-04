-- Seed: 17086812944581343788,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity fg is
  port (m : in std_logic_vector(2 to 0); dsodxxcg : in std_logic);
end fg;

architecture uzvxpybz of fg is
  
begin
  
end uzvxpybz;

entity fxfm is
  port (hwynz : inout time; eznk : buffer real; g : inout integer);
end fxfm;

library ieee;
use ieee.std_logic_1164.all;

architecture sbrjgjqc of fxfm is
  signal dopi : std_logic;
  signal kzu : std_logic_vector(2 to 0);
begin
  ixu : entity work.fg
    port map (m => kzu, dsodxxcg => dopi);
end sbrjgjqc;

entity vvewo is
  port (dygu : inout integer_vector(1 downto 1));
end vvewo;

library ieee;
use ieee.std_logic_1164.all;

architecture hhyugwtb of vvewo is
  signal rbt : std_logic_vector(2 to 0);
  signal rvkmdyz : std_logic;
  signal q : std_logic_vector(2 to 0);
begin
  xqmhjz : entity work.fg
    port map (m => q, dsodxxcg => rvkmdyz);
  aeme : entity work.fg
    port map (m => rbt, dsodxxcg => rvkmdyz);
  
  -- Single-driven assignments
  dygu <= dygu;
  
  -- Multi-driven assignments
  q <= q;
  q <= q;
  q <= "";
  rbt <= q;
end hhyugwtb;



-- Seed after: 6578192064449888995,4404421571376382767
