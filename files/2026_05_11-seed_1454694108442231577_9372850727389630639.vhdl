-- Seed: 1454694108442231577,9372850727389630639



entity qezw is
  port (igziw : in integer);
end qezw;



architecture zorl of qezw is
  
begin
  
end zorl;



entity ppi is
  port (cp : buffer integer);
end ppi;



architecture ercsk of ppi is
  signal p : integer;
begin
  b : entity work.qezw
    port map (igziw => cp);
  lxk : entity work.qezw
    port map (igziw => p);
  qfbmom : entity work.qezw
    port map (igziw => p);
end ercsk;



-- Seed after: 16437493629116888575,9372850727389630639
