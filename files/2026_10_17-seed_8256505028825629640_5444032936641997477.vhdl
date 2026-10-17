-- Seed: 8256505028825629640,5444032936641997477

entity cs is
  port (ynjdowdpwl : linkage bit_vector(4 to 1); gwrbpkxq : out boolean; jefsvm : in time);
end cs;

architecture qdvmhrm of cs is
  
begin
  -- Single-driven assignments
  gwrbpkxq <= gwrbpkxq;
end qdvmhrm;

entity yehwjtqi is
  port (cpkpemboxa : in time);
end yehwjtqi;

architecture uaylmvbm of yehwjtqi is
  signal rpeujg : time;
  signal fstpyggsyi : boolean;
  signal x : bit_vector(4 to 1);
  signal rknyhc : boolean;
  signal lrkyowreg : bit_vector(4 to 1);
begin
  hluupre : entity work.cs
    port map (ynjdowdpwl => lrkyowreg, gwrbpkxq => rknyhc, jefsvm => cpkpemboxa);
  rvwruqkm : entity work.cs
    port map (ynjdowdpwl => x, gwrbpkxq => fstpyggsyi, jefsvm => rpeujg);
  
  -- Single-driven assignments
  rpeujg <= 2#0_1_0_1_1# fs;
end uaylmvbm;



-- Seed after: 4784778930814978044,5444032936641997477
