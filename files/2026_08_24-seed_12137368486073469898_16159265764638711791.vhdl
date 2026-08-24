-- Seed: 12137368486073469898,16159265764638711791

entity bgrg is
  port (lktxzm : in real_vector(2 to 3); cxhbrj : buffer time);
end bgrg;

architecture ywjme of bgrg is
  
begin
  -- Single-driven assignments
  cxhbrj <= 44.303 fs;
end ywjme;

entity mr is
  port (qjpudemuom : linkage severity_level);
end mr;

architecture c of mr is
  signal wjuafuuqm : time;
  signal zqlnmn : real_vector(2 to 3);
  signal gsd : time;
  signal w : real_vector(2 to 3);
  signal fwmjfvm : time;
  signal sabyxs : real_vector(2 to 3);
begin
  qmtwrgjdut : entity work.bgrg
    port map (lktxzm => sabyxs, cxhbrj => fwmjfvm);
  vt : entity work.bgrg
    port map (lktxzm => w, cxhbrj => gsd);
  vthmpr : entity work.bgrg
    port map (lktxzm => zqlnmn, cxhbrj => wjuafuuqm);
  
  -- Single-driven assignments
  w <= sabyxs;
end c;



-- Seed after: 16770050680372061546,16159265764638711791
