-- Seed: 2314143237796818840,15306146742274979323

entity cpinw is
  port (zi : buffer time);
end cpinw;

architecture vmd of cpinw is
  
begin
  -- Single-driven assignments
  zi <= 0.1_1_3 ns;
end vmd;



-- Seed after: 13208304990362673784,15306146742274979323
