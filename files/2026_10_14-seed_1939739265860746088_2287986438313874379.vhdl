-- Seed: 1939739265860746088,2287986438313874379

entity brj is
  port (zjjd : buffer boolean_vector(0 to 3));
end brj;

architecture vfgkrbawfl of brj is
  
begin
  -- Single-driven assignments
  zjjd <= (TRUE, FALSE, TRUE, TRUE);
end vfgkrbawfl;

entity yvz is
  port (hymrx : in real_vector(3 downto 0));
end yvz;

architecture rkbcshigg of yvz is
  signal rvumi : boolean_vector(0 to 3);
  signal drjqkvpp : boolean_vector(0 to 3);
  signal sfspucuycg : boolean_vector(0 to 3);
  signal odbzzs : boolean_vector(0 to 3);
begin
  fzteyd : entity work.brj
    port map (zjjd => odbzzs);
  fpdyxml : entity work.brj
    port map (zjjd => sfspucuycg);
  kennxqycix : entity work.brj
    port map (zjjd => drjqkvpp);
  lkfzorlln : entity work.brj
    port map (zjjd => rvumi);
end rkbcshigg;



-- Seed after: 4422176260386495821,2287986438313874379
