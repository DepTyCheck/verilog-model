-- Seed: 16630971216463402740,8891552411914730853

entity lbfyz is
  port (kdokel : linkage time_vector(1 to 3));
end lbfyz;

architecture xuhrb of lbfyz is
  
begin
  
end xuhrb;

entity drxhmg is
  port (ruetkhwgrw : in real);
end drxhmg;

architecture jj of drxhmg is
  signal qkvdd : time_vector(1 to 3);
  signal ld : time_vector(1 to 3);
begin
  l : entity work.lbfyz
    port map (kdokel => ld);
  pwrox : entity work.lbfyz
    port map (kdokel => qkvdd);
end jj;

entity mz is
  port (noosov : in time);
end mz;

architecture zng of mz is
  signal zbohk : time_vector(1 to 3);
  signal venwkwr : real;
  signal aeq : time_vector(1 to 3);
  signal vvnyij : time_vector(1 to 3);
begin
  idwxmqv : entity work.lbfyz
    port map (kdokel => vvnyij);
  i : entity work.lbfyz
    port map (kdokel => aeq);
  elidspryj : entity work.drxhmg
    port map (ruetkhwgrw => venwkwr);
  ljutvrwncq : entity work.lbfyz
    port map (kdokel => zbohk);
  
  -- Single-driven assignments
  venwkwr <= 41.0_0_0;
end zng;



-- Seed after: 1247580731674773064,8891552411914730853
