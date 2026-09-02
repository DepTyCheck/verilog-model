-- Seed: 8956442593087557856,3400751927341804175

entity dnsafsgw is
  port (tjuarqqt : in real; fkycsmgb : in time_vector(0 to 4); jdiswxh : out time; zovxyjgtvf : out real);
end dnsafsgw;

architecture nxiwpsbmnm of dnsafsgw is
  
begin
  -- Single-driven assignments
  jdiswxh <= jdiswxh;
  zovxyjgtvf <= zovxyjgtvf;
end nxiwpsbmnm;

entity phwru is
  port (gbpevzujl : linkage integer; zvagscu : buffer integer_vector(0 to 0); ergewx : out real);
end phwru;

architecture oegtkopt of phwru is
  signal dugoy : time;
  signal uoespzo : time_vector(0 to 4);
  signal rqyjjf : time;
  signal mzdyt : real;
  signal qjwrq : time;
  signal lgeco : time_vector(0 to 4);
  signal mnoxn : real;
begin
  sordw : entity work.dnsafsgw
    port map (tjuarqqt => mnoxn, fkycsmgb => lgeco, jdiswxh => qjwrq, zovxyjgtvf => mzdyt);
  hzjlkvyfra : entity work.dnsafsgw
    port map (tjuarqqt => mnoxn, fkycsmgb => lgeco, jdiswxh => rqyjjf, zovxyjgtvf => ergewx);
  mywvynf : entity work.dnsafsgw
    port map (tjuarqqt => mzdyt, fkycsmgb => uoespzo, jdiswxh => dugoy, zovxyjgtvf => mnoxn);
  
  -- Single-driven assignments
  zvagscu <= zvagscu;
  lgeco <= (16#A_1_6# ns, 41110 fs, 2#1_0_0_1# fs, 2#0_0# ms, 3_3_1_1_0 fs);
  uoespzo <= lgeco;
end oegtkopt;



-- Seed after: 6362392501924070983,3400751927341804175
