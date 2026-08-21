-- Seed: 2568192509151283115,16188444798499499427

entity p is
  port (hqwcp : out time);
end p;

architecture rexil of p is
  
begin
  -- Single-driven assignments
  hqwcp <= hqwcp;
end rexil;

entity c is
  port (gi : in bit; cur : buffer integer_vector(0 downto 4));
end c;

architecture ta of c is
  signal h : time;
begin
  ofnfjruxr : entity work.p
    port map (hqwcp => h);
  
  -- Single-driven assignments
  cur <= cur;
end ta;



-- Seed after: 13599237676809981843,16188444798499499427
