-- Seed: 16994365524264134799,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity fyrf is
  port (nr : linkage std_logic_vector(3 downto 2); zhjfohp : linkage bit; sbcl : linkage integer; kxncodeyj : in integer_vector(4 to 2));
end fyrf;

architecture juqv of fyrf is
  
begin
  
end juqv;

library ieee;
use ieee.std_logic_1164.all;

entity pkeiyl is
  port (k : buffer time; qfvltwtl : inout std_logic_vector(0 to 0));
end pkeiyl;

architecture jendkkr of pkeiyl is
  
begin
  -- Single-driven assignments
  k <= k;
  
  -- Multi-driven assignments
  qfvltwtl <= "Z";
  qfvltwtl <= (others => 'L');
  qfvltwtl <= qfvltwtl;
end jendkkr;

entity zntvytd is
  port (t : buffer severity_level);
end zntvytd;

library ieee;
use ieee.std_logic_1164.all;

architecture cy of zntvytd is
  signal v : integer;
  signal pywvxlngdi : bit;
  signal ezodqalj : integer_vector(4 to 2);
  signal yly : integer;
  signal fjfxs : bit;
  signal esmwcl : std_logic_vector(3 downto 2);
begin
  qo : entity work.fyrf
    port map (nr => esmwcl, zhjfohp => fjfxs, sbcl => yly, kxncodeyj => ezodqalj);
  xepqmzqf : entity work.fyrf
    port map (nr => esmwcl, zhjfohp => pywvxlngdi, sbcl => v, kxncodeyj => ezodqalj);
  
  -- Single-driven assignments
  t <= FAILURE;
end cy;



-- Seed after: 8697737164806169578,8412319452373742525
