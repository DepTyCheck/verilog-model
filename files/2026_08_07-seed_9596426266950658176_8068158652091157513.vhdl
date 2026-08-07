-- Seed: 9596426266950658176,8068158652091157513

entity bcpvbtl is
  port (xzlo : buffer severity_level; v : inout real);
end bcpvbtl;

architecture h of bcpvbtl is
  
begin
  -- Single-driven assignments
  v <= v;
  xzlo <= xzlo;
end h;



-- Seed after: 2078530423986364261,8068158652091157513
