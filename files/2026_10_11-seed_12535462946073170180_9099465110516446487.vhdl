-- Seed: 12535462946073170180,9099465110516446487

entity jlozds is
  port (ztnu : buffer time_vector(0 to 1));
end jlozds;

architecture jri of jlozds is
  
begin
  -- Single-driven assignments
  ztnu <= (8#7407.73# ms, 4 min);
end jri;

entity fjk is
  port (ltuam : out boolean);
end fjk;

architecture tslkrvd of fjk is
  signal agkrug : time_vector(0 to 1);
  signal hypk : time_vector(0 to 1);
  signal txrlbar : time_vector(0 to 1);
begin
  mjz : entity work.jlozds
    port map (ztnu => txrlbar);
  l : entity work.jlozds
    port map (ztnu => hypk);
  newogw : entity work.jlozds
    port map (ztnu => agkrug);
end tslkrvd;



-- Seed after: 17106237911342221078,9099465110516446487
