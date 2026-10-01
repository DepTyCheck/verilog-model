-- Seed: 3480033137989241204,15025465285671019065

entity nc is
  port (ftleppcw : buffer integer; ucclgofxwi : in time; ysd : inout real; rjtfqqmon : out real_vector(0 to 2));
end nc;

architecture sls of nc is
  
begin
  -- Single-driven assignments
  rjtfqqmon <= (010.1_4_4_2_2, 16#2_0.C_2_D#, 8#5.00#);
end sls;

entity qcxqvpfnw is
  port (g : buffer real; szhgaj : inout real; eir : in time; dgwg : out real);
end qcxqvpfnw;

architecture fkxw of qcxqvpfnw is
  signal nzokdjsdyz : real_vector(0 to 2);
  signal we : time;
  signal ulerksmio : integer;
  signal cqnjaxqp : real_vector(0 to 2);
  signal oknvf : real;
  signal sqmybndvyu : time;
  signal cnm : integer;
  signal bmwtjys : real_vector(0 to 2);
  signal qpzqw : real;
  signal gsulwhv : time;
  signal ve : integer;
  signal yqa : real_vector(0 to 2);
  signal ilxypvkw : real;
  signal kwpkyx : time;
  signal fvkipfcg : integer;
begin
  uvtyygyrq : entity work.nc
    port map (ftleppcw => fvkipfcg, ucclgofxwi => kwpkyx, ysd => ilxypvkw, rjtfqqmon => yqa);
  jac : entity work.nc
    port map (ftleppcw => ve, ucclgofxwi => gsulwhv, ysd => qpzqw, rjtfqqmon => bmwtjys);
  m : entity work.nc
    port map (ftleppcw => cnm, ucclgofxwi => sqmybndvyu, ysd => oknvf, rjtfqqmon => cqnjaxqp);
  kpi : entity work.nc
    port map (ftleppcw => ulerksmio, ucclgofxwi => we, ysd => dgwg, rjtfqqmon => nzokdjsdyz);
end fkxw;

entity vweelp is
  port (byjdaxw : inout time; iulfpytz : in real; yrsbvkx : inout string(5 to 3); jvzg : out bit);
end vweelp;

architecture wvuvi of vweelp is
  signal pwxnzou : real;
  signal aerny : real;
  signal wvlzgdjh : real;
begin
  qi : entity work.qcxqvpfnw
    port map (g => wvlzgdjh, szhgaj => aerny, eir => byjdaxw, dgwg => pwxnzou);
  
  -- Single-driven assignments
  jvzg <= '1';
  yrsbvkx <= (others => ' ');
  byjdaxw <= byjdaxw;
end wvuvi;



-- Seed after: 16571611044688600630,15025465285671019065
