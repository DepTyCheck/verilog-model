-- Seed: 12655651293721179361,5805648483995786113

entity bb is
  port (pbi : out string(2 downto 1));
end bb;

architecture zzgvmyc of bb is
  
begin
  -- Single-driven assignments
  pbi <= "fl";
end zzgvmyc;

library ieee;
use ieee.std_logic_1164.all;

entity dorbyiy is
  port (icaxdrf : buffer bit_vector(3 downto 1); mruzbgumx : inout std_logic);
end dorbyiy;

architecture dfhrmrxv of dorbyiy is
  signal vxsrvl : string(2 downto 1);
begin
  epfbuabhc : entity work.bb
    port map (pbi => vxsrvl);
  
  -- Multi-driven assignments
  mruzbgumx <= 'L';
  mruzbgumx <= mruzbgumx;
end dfhrmrxv;



-- Seed after: 7548596062419262227,5805648483995786113
