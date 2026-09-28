-- Seed: 7330985443664751438,7311216359267151659

entity khzicjqdn is
  port (vahwsvsg : inout boolean_vector(2 to 2));
end khzicjqdn;

architecture mkl of khzicjqdn is
  
begin
  -- Single-driven assignments
  vahwsvsg <= (others => FALSE);
end mkl;

entity kvhodl is
  port (oxv : buffer time; yyl : out boolean);
end kvhodl;

architecture ao of kvhodl is
  signal qsdtycnhiw : boolean_vector(2 to 2);
  signal vhfvumlair : boolean_vector(2 to 2);
  signal nuslyfc : boolean_vector(2 to 2);
begin
  zg : entity work.khzicjqdn
    port map (vahwsvsg => nuslyfc);
  rdd : entity work.khzicjqdn
    port map (vahwsvsg => vhfvumlair);
  zn : entity work.khzicjqdn
    port map (vahwsvsg => qsdtycnhiw);
  
  -- Single-driven assignments
  yyl <= TRUE;
end ao;



-- Seed after: 17359342670670935169,7311216359267151659
