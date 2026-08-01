-- Seed: 6696697218642621722,4292249356257567981

entity irywqza is
  port (cua : linkage real);
end irywqza;

architecture gatngpj of irywqza is
  
begin
  
end gatngpj;

entity cglix is
  port (jsc : in boolean_vector(3 to 2); rqsqvw : out time; mmrebhfwl : out boolean_vector(4 to 3));
end cglix;

architecture x of cglix is
  signal vtptrtber : real;
  signal bmicmqboa : real;
  signal xmz : real;
begin
  fn : entity work.irywqza
    port map (cua => xmz);
  ynrol : entity work.irywqza
    port map (cua => bmicmqboa);
  qvpsu : entity work.irywqza
    port map (cua => vtptrtber);
  
  -- Single-driven assignments
  mmrebhfwl <= (others => TRUE);
  rqsqvw <= rqsqvw;
end x;

entity y is
  port (rn : inout time);
end y;

architecture edzljph of y is
  signal tqfvlrccde : boolean_vector(4 to 3);
  signal z : time;
  signal smrds : boolean_vector(4 to 3);
begin
  pqlqsmwmh : entity work.cglix
    port map (jsc => smrds, rqsqvw => z, mmrebhfwl => tqfvlrccde);
  a : entity work.cglix
    port map (jsc => smrds, rqsqvw => rn, mmrebhfwl => smrds);
end edzljph;



-- Seed after: 8437483032037816152,4292249356257567981
