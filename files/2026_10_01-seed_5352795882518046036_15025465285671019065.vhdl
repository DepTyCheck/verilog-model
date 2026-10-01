-- Seed: 5352795882518046036,15025465285671019065

entity qnt is
  port (vlgtwcpap : linkage integer_vector(3 downto 2); aocluocsw : buffer time);
end qnt;

architecture bjfdpwejxb of qnt is
  
begin
  -- Single-driven assignments
  aocluocsw <= aocluocsw;
end bjfdpwejxb;

entity tibmtsbsxc is
  port (urazyoei : linkage real; pw : linkage time_vector(2 downto 2));
end tibmtsbsxc;

architecture jysweode of tibmtsbsxc is
  signal d : time;
  signal snwts : integer_vector(3 downto 2);
  signal row : time;
  signal dntqnooka : integer_vector(3 downto 2);
begin
  nlydgnl : entity work.qnt
    port map (vlgtwcpap => dntqnooka, aocluocsw => row);
  wlgduog : entity work.qnt
    port map (vlgtwcpap => snwts, aocluocsw => d);
end jysweode;

entity qbkfvha is
  port (jfxbxyryzh : buffer bit; lgf : inout time; pbq : linkage integer; vx : out character);
end qbkfvha;

architecture lt of qbkfvha is
  signal vcndebt : time;
  signal zbrpi : integer_vector(3 downto 2);
  signal v : integer_vector(3 downto 2);
  signal brmec : time;
  signal zqnnibu : integer_vector(3 downto 2);
  signal ofkoigzbbw : time_vector(2 downto 2);
  signal qg : real;
begin
  pmg : entity work.tibmtsbsxc
    port map (urazyoei => qg, pw => ofkoigzbbw);
  ma : entity work.qnt
    port map (vlgtwcpap => zqnnibu, aocluocsw => brmec);
  sevrerbu : entity work.qnt
    port map (vlgtwcpap => v, aocluocsw => lgf);
  hzd : entity work.qnt
    port map (vlgtwcpap => zbrpi, aocluocsw => vcndebt);
  
  -- Single-driven assignments
  jfxbxyryzh <= jfxbxyryzh;
  vx <= 'z';
end lt;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (vfcyfhap : out time; sqyyoks : linkage std_logic);
end b;

architecture yw of b is
  signal eahh : time;
  signal o : integer_vector(3 downto 2);
  signal oterzigrhn : integer_vector(3 downto 2);
begin
  zrmkn : entity work.qnt
    port map (vlgtwcpap => oterzigrhn, aocluocsw => vfcyfhap);
  tdz : entity work.qnt
    port map (vlgtwcpap => o, aocluocsw => eahh);
end yw;



-- Seed after: 4554023660482863513,15025465285671019065
