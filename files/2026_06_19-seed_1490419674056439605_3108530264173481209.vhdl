-- Seed: 1490419674056439605,3108530264173481209

entity pdfstauyej is
  port (tsvthrf : in real);
end pdfstauyej;

architecture aizi of pdfstauyej is
  
begin
  
end aizi;

entity urycz is
  port (w : inout time; fwzjnqy : in real);
end urycz;

architecture fiq of urycz is
  signal hjgfsdx : real;
begin
  qju : entity work.pdfstauyej
    port map (tsvthrf => hjgfsdx);
  zgkdzd : entity work.pdfstauyej
    port map (tsvthrf => hjgfsdx);
  frqb : entity work.pdfstauyej
    port map (tsvthrf => fwzjnqy);
  
  -- Single-driven assignments
  w <= 0 min;
  hjgfsdx <= 16#1_1_F_C_7.7#;
end fiq;

entity zkbi is
  port (fb : inout real_vector(2 to 1); u : inout real; r : out severity_level);
end zkbi;

architecture xiq of zkbi is
  signal ylkfk : real;
begin
  lo : entity work.pdfstauyej
    port map (tsvthrf => u);
  jyd : entity work.pdfstauyej
    port map (tsvthrf => ylkfk);
end xiq;



-- Seed after: 13959871781984179706,3108530264173481209
