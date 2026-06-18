-- Seed: 4000784701706636402,8118127366649987907

entity osdyloeq is
  port (bw : linkage real);
end osdyloeq;

architecture l of osdyloeq is
  
begin
  
end l;

entity rc is
  port (jsnhjxxg : buffer real; gyfousj : inout character; hrkzil : out integer; dm : out time);
end rc;

architecture xznewjdn of rc is
  signal ylxiaktk : real;
  signal owu : real;
begin
  cccxpeowm : entity work.osdyloeq
    port map (bw => owu);
  vcqea : entity work.osdyloeq
    port map (bw => ylxiaktk);
  iayazjqys : entity work.osdyloeq
    port map (bw => jsnhjxxg);
  
  -- Single-driven assignments
  dm <= 2#00.1# fs;
  hrkzil <= 022;
  gyfousj <= 'q';
end xznewjdn;



-- Seed after: 7194048009640963781,8118127366649987907
