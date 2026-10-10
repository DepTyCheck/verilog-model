-- Seed: 9903615090044308469,511364357853360275

entity ugrimmecpf is
  port (onjmhutrl : buffer real);
end ugrimmecpf;

architecture gl of ugrimmecpf is
  
begin
  -- Single-driven assignments
  onjmhutrl <= onjmhutrl;
end gl;

entity acbhoehpez is
  port (qdevyttq : buffer boolean_vector(2 to 2));
end acbhoehpez;

architecture rxkqro of acbhoehpez is
  signal cfonbxq : real;
  signal cykmyvkev : real;
begin
  scxefu : entity work.ugrimmecpf
    port map (onjmhutrl => cykmyvkev);
  j : entity work.ugrimmecpf
    port map (onjmhutrl => cfonbxq);
  
  -- Single-driven assignments
  qdevyttq <= qdevyttq;
end rxkqro;

entity oihyuvien is
  port (nrctdqfg : in real; ribyfcko : in time; anvo : inout real; i : in real);
end oihyuvien;

architecture qqaggw of oihyuvien is
  signal nafvfmtnl : real;
  signal r : boolean_vector(2 to 2);
  signal zxvdobzi : real;
begin
  jsv : entity work.ugrimmecpf
    port map (onjmhutrl => zxvdobzi);
  dasxhypte : entity work.ugrimmecpf
    port map (onjmhutrl => anvo);
  xsni : entity work.acbhoehpez
    port map (qdevyttq => r);
  fdwibcvg : entity work.ugrimmecpf
    port map (onjmhutrl => nafvfmtnl);
end qqaggw;

entity it is
  port (cruivtybw : linkage real; edkuzcqon : in integer_vector(0 to 1); rstj : inout time);
end it;

architecture iqu of it is
  signal vc : real;
  signal sleul : real;
  signal mtwipqaeg : real;
begin
  mc : entity work.oihyuvien
    port map (nrctdqfg => mtwipqaeg, ribyfcko => rstj, anvo => sleul, i => vc);
  
  -- Single-driven assignments
  rstj <= 0_0_4 us;
end iqu;



-- Seed after: 4476677830716771834,511364357853360275
